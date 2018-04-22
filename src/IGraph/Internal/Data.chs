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
    , asBS
    , bsToByteString
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
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
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
    my_igraph_vector_destroy newtype#}

-- Construtors and destructors

allocaVector :: (Ptr Vector -> IO a) -> IO a
allocaVector f = mallocBytes {# sizeof igraph_vector_t #} >>= f
{-# INLINE allocaVector #-}

addVectorFinalizer :: Ptr Vector -> IO Vector
addVectorFinalizer ptr = do
    vec <- newForeignPtr my_igraph_vector_destroy ptr
    return $ Vector vec
{-# INLINE addVectorFinalizer #-}

{#fun igraph_vector_init as igraphVectorNew
    { allocaVector- `Vector' addVectorFinalizer*
    , `Int' } -> `CInt' void- #}

{#fun igraph_vector_init_copy as ^
    { allocaVector- `Vector' addVectorFinalizer*
    , id `Ptr CDouble', `Int' } -> `CInt' void- #}

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

{#pointer *bytestring_t as BSLen foreign newtype#}

bsToByteString :: Ptr BSLen -> IO B.ByteString
bsToByteString ptr = do
    n <- {#get bytestring_t->len #} ptr
    str <- {#get bytestring_t->value #} ptr
    packCStringLen (str, fromIntegral n)
{-# INLINE bsToByteString #-}

asBS :: B.ByteString -> (Ptr BSLen -> IO a) -> IO a
asBS x f = unsafeUseAsCStringLen x $ \(str, n) -> do
    fptr <- mallocForeignPtrBytes {#sizeof bytestring_t #}
    withForeignPtr fptr $ \ptr -> do
        {#set bytestring_t.len #} ptr (fromIntegral n)
        {#set bytestring_t.value #} ptr str
        f ptr
{-# INLINE asBS #-}

{#pointer *bsvector_t as BSVector foreign finalizer bsvector_destroy newtype#}

allocaBSVector :: (Ptr BSVector -> IO a) -> IO a
allocaBSVector f = mallocBytes {# sizeof bsvector_t #} >>= f
{-# INLINE allocaBSVector #-}

addBSVectorFinalizer :: Ptr BSVector -> IO BSVector
addBSVectorFinalizer ptr = do
    vec <- newForeignPtr bsvector_destroy ptr
    return $ BSVector vec
{-# INLINE addBSVectorFinalizer #-}

{#fun bsvector_init as bsvectorNew
    { allocaBSVector- `BSVector' addBSVectorFinalizer*
    , `Int'
    } -> `CInt' void- #}

{#fun bsvector_set as bsvectorSet' { `BSVector', `Int', castPtr `Ptr BSLen' } -> `()' #}

bsvectorSet :: BSVector -> Int -> B.ByteString -> IO ()
bsvectorSet vec i bs = asBS bs (bsvectorSet' vec i)
{-# INLINE bsvectorSet #-}

toBSVector :: [B.ByteString] -> IO BSVector
toBSVector xs = do
    vec <- bsvectorNew n
    foldM_ (\i x -> bsvectorSet vec i x >> return (i+1)) 0 xs
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
