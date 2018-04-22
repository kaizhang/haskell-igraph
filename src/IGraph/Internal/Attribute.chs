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

{#pointer *igraph_attribute_record_t as AttributeRecord foreign newtype#}

withAttr :: String
         -> BSVector -> (Ptr AttributeRecord -> IO a) -> IO a
withAttr name bs f = withBSVector bs $ \ptr -> do
    fptr <- mallocForeignPtrBytes {#sizeof igraph_attribute_record_t #}
    withForeignPtr fptr $ \attr -> withCString name $ \name' -> do
        {#set igraph_attribute_record_t.name #} attr name'
        {#set igraph_attribute_record_t.type #} attr 2
        {#set igraph_attribute_record_t.value #} attr $ castPtr ptr
        f attr
{-# INLINE withAttr #-}

{#fun igraph_haskell_attribute_has_attr as ^ { `IGraph', `AttributeElemtype', `String' } -> `Bool' #}

{#fun igraph_haskell_attribute_GAN_set as ^ { `IGraph', `String', `Double' } -> `Int' #}

{#fun igraph_haskell_attribute_GAN as ^ { `IGraph', `String' } -> `Double' #}

{#fun igraph_haskell_attribute_VAS as ^ { `IGraph', `String', `Int' } -> `Ptr BSLen' castPtr #}

{#fun igraph_haskell_attribute_EAN as ^ { `IGraph', `String', `Int' } -> `Double' #}

{#fun igraph_haskell_attribute_EAS as ^ { `IGraph', `String', `Int' } -> `Ptr BSLen' castPtr #}

{#fun igraph_haskell_attribute_EAS_setv as ^ { `IGraph', `String', `BSVector' } -> `Int' #}

{#fun igraph_haskell_attribute_VAS_set as ^ { `IGraph', `String', `Int', castPtr `Ptr BSLen' } -> `Int' #}

{#fun igraph_haskell_attribute_EAS_set as ^ { `IGraph', `String', `Int', castPtr `Ptr BSLen' } -> `Int' #}
