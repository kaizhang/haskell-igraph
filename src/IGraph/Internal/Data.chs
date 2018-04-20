{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Data
    ( Vector(..)
    , withVector
    , igraphVectorNew
    , fromList
    , toList
    , igraphVectorNull
    , igraphVectorFill
    , igraphVectorE
    , igraphVectorSet
    , igraphVectorTail
    , igraphVectorSize
    , igraphVectorCopyTo

    , VectorPtr(..)
    , withVectorPtr
    , igraphVectorPtrNew
    , fromPtrs
    , toLists

    , StrVector(..)
    , withStrVector
    , igraphStrvectorNew
    , igraphStrvectorGet
    , toStrVector

    , BSLen(..)
    , BSVector(..)
    , withBSVector
    , bsvectorNew
    , bsvectorSet
    , toBSVector

    , Matrix(..)
    , withMatrix
    , igraphMatrixNew
    , igraphMatrixNull
    , igraphMatrixFill
    , igraphMatrixE
    , igraphMatrixSet
    , igraphMatrixCopyTo
    , igraphMatrixNrow
    , igraphMatrixNcol
    , fromRowLists
    , toRowLists
    , toColumnLists
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.List (transpose)
import Data.List.Split (chunksOf)

#include "haskell_igraph.h"
#include "bytestring.h"

--------------------------------------------------------------------------------
-- Igraph vector
--------------------------------------------------------------------------------

{#pointer *igraph_vector_t as Vector foreign finalizer
    igraph_vector_destroy newtype#}

-- Construtors and destructors

{#fun igraph_vector_init as igraphVectorNew { +, `Int' } -> `Vector' #}

{#fun igraph_vector_init_copy as ^ { +, id `Ptr CDouble', `Int' } -> `Vector' #}

fromList :: [Double] -> IO Vector
fromList xs = withArrayLen (map realToFrac xs) $ \n ptr ->
    igraphVectorInitCopy ptr n
{-# INLINE fromList #-}

toList :: Vector -> IO [Double]
toList vec = do
    n <- igraphVectorSize vec
    allocaArray n $ \ptr -> do
        igraphVectorCopyTo vec ptr
        liftM (map realToFrac) $ peekArray n ptr
{-# INLINE toList #-}

-- Initializing elements

{#fun igraph_vector_null as ^ { `Vector' } -> `()' #}

{#fun igraph_vector_fill as ^ { `Vector', `Double' } -> `()' #}


-- Accessing elements

{#fun pure igraph_vector_e as ^ { `Vector', `Int' } -> `Double' #}

{#fun igraph_vector_set as ^ { `Vector', `Int', `Double' } -> `()' #}

{#fun pure igraph_vector_tail as ^ { `Vector' } -> `Double' #}


-- Copying vectors

{#fun igraph_vector_copy_to as ^ { `Vector', id `Ptr CDouble' } -> `()' #}

-- Vector properties
{#fun igraph_vector_size as ^ { `Vector' } -> `Int' #}


{#pointer *igraph_vector_ptr_t as VectorPtr foreign finalizer
    igraph_vector_ptr_destroy_all newtype#}

{#fun igraph_vector_ptr_init as igraphVectorPtrNew { +, `Int' } -> `VectorPtr' #}

{#fun igraph_vector_ptr_e as ^ { `VectorPtr', `Int' } -> `Ptr ()' #}
{#fun igraph_vector_ptr_set as ^ { `VectorPtr', `Int', id `Ptr ()' } -> `()' #}
{#fun igraph_vector_ptr_size as ^ { `VectorPtr' } -> `Int' #}

fromPtrs :: [Ptr ()] -> IO VectorPtr
fromPtrs xs = do
    vptr <- igraphVectorPtrNew n
    forM_ (zip [0..] xs) $ \(i,x) -> igraphVectorPtrSet vptr i x
    return vptr
  where
    n = length xs
{-# INLINE fromPtrs #-}

toLists :: VectorPtr -> IO [[Double]]
toLists vpptr = do
    n <- igraphVectorPtrSize vpptr
    forM [0..n-1] $ \i -> do
        vptr <- igraphVectorPtrE vpptr i
        vec <- newForeignPtr_ $ castPtr vptr
        toList $ Vector vec
{-# INLINE toLists #-}

--------------------------------------------------------------------------------
-- Igraph string vector
--------------------------------------------------------------------------------

{#pointer *igraph_strvector_t as StrVector foreign finalizer igraph_strvector_destroy newtype#}

{#fun igraph_strvector_init as igraphStrvectorNew { +, `Int' } -> `StrVector' #}

{#fun igraph_strvector_get as ^
    { `StrVector'
    , `Int'
    , alloca- `String' peekString*
    } -> `CInt' void-#}

peekString :: Ptr CString -> IO String
peekString ptr = peek ptr >>= peekCString
{-# INLINE peekString #-}

{#fun igraph_strvector_set as ^ { `StrVector', `Int', id `CString'} -> `()' #}
{#fun igraph_strvector_set2 as ^ { `StrVector', `Int', id `CString', `Int'} -> `()' #}

toStrVector :: [B.ByteString] -> IO StrVector
toStrVector xs = do
    vec <- igraphStrvectorNew n
    forM_ (zip [0..] xs) $ \(i,x) -> B.useAsCString x (igraphStrvectorSet vec i)
    return vec
  where
    n = length xs


--------------------------------------------------------------------------------
-- Customized string vector
--------------------------------------------------------------------------------

newtype BSLen = BSLen CStringLen

instance Storable BSLen where
    sizeOf _ = {#sizeof bytestring_t #}
    alignment _ = {#alignof bytestring_t #}
    peek p = do
        n <- ({#get bytestring_t->len #} p)
        ptr <- {#get bytestring_t->value #} p
        return $ BSLen (ptr, fromIntegral n)
    poke p (BSLen (ptr, n)) = {#set bytestring_t.len #} p (fromIntegral n) >>
        {#set bytestring_t.value #} p ptr

{#pointer *bsvector_t as BSVector foreign finalizer bsvector_destroy newtype#}

{#fun bsvector_init as bsvectorNew { +, `Int' } -> `BSVector' #}

--{#fun bsvector_get as bsVectorGet { `BSVectorPtr', `Int', + } -> `Ptr (Ptr BSLen)' id #}

{-
bsVectorGet :: BSVectorPtr -> Int -> BSLen
bsVectorGet vec i = unsafePerformIO $ do
    ptrptr <- bsVectorGet vec i
    peek ptrptr >>= peek
    -}

{#fun bsvector_set as ^ { `BSVector', `Int', `Ptr ()'} -> `()' #}

toBSVector :: [BSLen] -> IO BSVector
toBSVector xs = do
    vec <- bsvectorNew n
    forM_ (zip [0..] xs) $ \(i, x) -> with x $ \ptr -> bsvectorSet vec i $ castPtr ptr
    return vec
  where
    n = length xs


{#pointer *igraph_matrix_t as Matrix foreign finalizer igraph_matrix_destroy newtype#}

{#fun igraph_matrix_init as igraphMatrixNew { +, `Int', `Int' } -> `Matrix' #}

{#fun igraph_matrix_null as ^ { `Matrix' } -> `()' #}

{#fun igraph_matrix_fill as ^ { `Matrix', `Double' } -> `()' #}

{#fun igraph_matrix_e as ^ { `Matrix', `Int', `Int' } -> `Double' #}

{#fun igraph_matrix_set as ^ { `Matrix', `Int', `Int', `Double' } -> `()' #}

{#fun igraph_matrix_copy_to as ^ { `Matrix', id `Ptr CDouble' } -> `()' #}

{#fun igraph_matrix_nrow as ^ { `Matrix' } -> `Int' #}

{#fun igraph_matrix_ncol as ^ { `Matrix' } -> `Int' #}

-- row lists to matrix
fromRowLists :: [[Double]] -> IO Matrix
fromRowLists xs
    | all (==c) $ map length xs = do
        mptr <- igraphMatrixNew r c
        forM_ (zip [0..] xs) $ \(i, row) ->
            forM_ (zip [0..] row) $ \(j,v) ->
                igraphMatrixSet mptr i j v
        return mptr
    | otherwise = error "Not a matrix."
  where
    r = length xs
    c = length $ head xs

-- to row lists
toRowLists :: Matrix -> IO [[Double]]
toRowLists = liftM transpose . toColumnLists

toColumnLists :: Matrix -> IO [[Double]]
toColumnLists mptr = do
    r <- igraphMatrixNrow mptr
    c <- igraphMatrixNcol mptr
    xs <- allocaArray (r*c) $ \ptr -> do
        igraphMatrixCopyTo mptr ptr
        peekArray (r*c) ptr
    return $ chunksOf r $ map realToFrac xs
