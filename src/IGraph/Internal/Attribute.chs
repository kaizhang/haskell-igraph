{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Attribute where

import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Control.Monad
import Control.Applicative
import Data.Serialize (Serialize, decode, encode)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}
{#import IGraph.Internal.Constants #}

#include "igraph/igraph.h"
#include "haskell_attributes.h"

-- The returned object will not be trackced by Haskell's GC. It should be freed
-- by foreign codes.
asBS :: Serialize a => a -> (BSLen -> IO b) -> IO b
asBS x fn = unsafeUseAsCStringLen (encode x) (fn . BSLen)
{-# INLINE asBS #-}

asBSVector :: Serialize a => [a] -> (BSVectorPtr -> IO b) -> IO b
asBSVector values fn = loop [] values
  where
    loop acc (x:xs) = unsafeUseAsCStringLen (encode x) $ \ptr ->
        loop (BSLen ptr : acc) xs
    loop acc _ = listToBSVector (reverse acc) >>= fn
{-# INLINE asBSVector #-}

fromBS :: Serialize a => Ptr BSLen -> IO a
fromBS ptr = do
    BSLen x <- peek ptr
    result <- decode <$> packCStringLen x
    case result of
        Left msg -> error msg
        Right r -> return r
{-# INLINE fromBS #-}

mkStrRec :: CString    -- ^ name of the attribute
         -> BSVectorPtr       -- ^ values of the attribute
         -> AttributeRecord
mkStrRec name xs = AttributeRecord name 2 xs
{-# INLINE mkStrRec #-}

data AttributeRecord = AttributeRecord CString Int BSVectorPtr

instance Storable AttributeRecord where
    sizeOf _ = {#sizeof igraph_attribute_record_t #}
    alignment _ = {#alignof igraph_attribute_record_t #}
    peek p = AttributeRecord
        <$> ({#get igraph_attribute_record_t->name #} p)
        <*> liftM fromIntegral ({#get igraph_attribute_record_t->type #} p)
        <*> ( do ptr <- {#get igraph_attribute_record_t->value #} p
                 fptr <- newForeignPtr_ . castPtr $ ptr
                 return $ BSVectorPtr fptr )
    poke p (AttributeRecord name t vptr) = do
        {#set igraph_attribute_record_t.name #} p name
        {#set igraph_attribute_record_t.type #} p $ fromIntegral t
        withBSVectorPtr vptr $ \ptr ->
            {#set igraph_attribute_record_t.value #} p $ castPtr ptr

{#fun pure igraph_haskell_attribute_has_attr as ^ { `IGraphPtr', `AttributeElemtype', `String' } -> `Bool' #}

{#fun igraph_haskell_attribute_GAN_set as ^ { `IGraphPtr', `String', `Double' } -> `Int' #}

{#fun pure igraph_haskell_attribute_GAN as ^ { `IGraphPtr', `String' } -> `Double' #}

{#fun igraph_haskell_attribute_VAS as ^ { `IGraphPtr', `String', `Int' } -> `Ptr BSLen' castPtr #}

{#fun pure igraph_haskell_attribute_EAN as ^ { `IGraphPtr', `String', `Int' } -> `Double' #}

{#fun igraph_haskell_attribute_EAS as ^ { `IGraphPtr', `String', `Int' } -> `Ptr BSLen' castPtr #}

{#fun igraph_haskell_attribute_EAS_setv as ^ { `IGraphPtr', `String', `BSVectorPtr' } -> `Int' #}

{#fun igraph_haskell_attribute_VAS_set as ^ { `IGraphPtr', `String', `Int', castPtr `Ptr BSLen' } -> `Int' #}

{#fun igraph_haskell_attribute_EAS_set as ^ { `IGraphPtr', `String', `Int', castPtr `Ptr BSLen' } -> `Int' #}
