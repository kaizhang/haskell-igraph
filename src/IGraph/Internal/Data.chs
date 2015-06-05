{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Data where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.List (transpose)
import Data.List.Split (chunksOf)

#include "cbits/haskelligraph.c"

{#pointer *igraph_vector_t as VectorPtr foreign finalizer igraph_vector_destroy newtype#}

-- Construtors and destructors

{#fun igraph_vector_init as igraphVectorNew { +, `Int' } -> `VectorPtr' #}

listToVector :: [Double] -> IO VectorPtr
listToVector xs = do
    vec <- igraphVectorNew n
    forM_ (zip [0..] xs) $ \(i,x) -> igraphVectorSet vec i x
    return vec
  where
    n = length xs

vectorPtrToList :: VectorPtr -> IO [Double]
vectorPtrToList vptr = do
    n <- igraphVectorSize vptr
    allocaArray n $ \ptr -> do
        igraphVectorCopyTo vptr ptr
        liftM (map realToFrac) $ peekArray n ptr

-- Initializing elements

{#fun igraph_vector_null as ^ { `VectorPtr' } -> `()' #}

{#fun igraph_vector_fill as ^ { `VectorPtr', `Double' } -> `()' #}


-- Accessing elements

{#fun pure igraph_vector_e as ^ { `VectorPtr', `Int' } -> `Double' #}

{#fun igraph_vector_set as ^ { `VectorPtr', `Int', `Double' } -> `()' #}

{#fun pure igraph_vector_tail as ^ { `VectorPtr' } -> `Double' #}


-- Copying vectors

{#fun igraph_vector_copy_to as ^ { `VectorPtr', id `Ptr CDouble' } -> `()' #}

-- Vector properties
{#fun igraph_vector_size as ^ { `VectorPtr' } -> `Int' #}


{#pointer *igraph_vector_ptr_t as VectorPPtr foreign finalizer igraph_vector_ptr_destroy_all newtype#}

{#fun igraph_vector_ptr_init as igraphVectorPtrNew { +, `Int' } -> `VectorPPtr' #}

{#fun igraph_vector_ptr_e as ^ { `VectorPPtr', `Int' } -> `Ptr ()' #}
{#fun igraph_vector_ptr_set as ^ { `VectorPPtr', `Int', id `Ptr ()' } -> `()' #}
{#fun igraph_vector_ptr_size as ^ { `VectorPPtr' } -> `Int' #}

listToVectorP :: [Ptr ()] -> IO VectorPPtr
listToVectorP xs = do
    vptr <- igraphVectorPtrNew n
    forM_ (zip [0..] xs) $ \(i,x) -> igraphVectorPtrSet vptr i x
    return vptr
  where
    n = length xs

vectorPPtrToList :: VectorPPtr -> IO [[Double]]
vectorPPtrToList vpptr = do
    n <- igraphVectorPtrSize vpptr
    forM [0..n-1] $ \i -> do
        vptr <- igraphVectorPtrE vpptr i
        fptr <- newForeignPtr_ $ castPtr vptr
        vectorPtrToList $ VectorPtr fptr


{#pointer *igraph_strvector_t as StrVectorPtr foreign finalizer igraph_strvector_destroy newtype#}

{#fun igraph_strvector_init as igraphStrvectorNew { +, `Int' } -> `StrVectorPtr' #}

{#fun igraph_strvector_get_ as igraphStrvectorGet' { `StrVectorPtr', `Int' } -> `Ptr CString' id #}

igraphStrvectorGet :: StrVectorPtr -> Int -> String
igraphStrvectorGet vec i = unsafePerformIO $ do
    ptr <- igraphStrvectorGet' vec i
    peek ptr >>= peekCString

{#fun igraph_strvector_set as ^ { `StrVectorPtr', `Int', id `CString'} -> `()' #}
{#fun igraph_strvector_set2 as ^ { `StrVectorPtr', `Int', id `CString', `Int'} -> `()' #}

listToStrVector :: [B.ByteString] -> IO StrVectorPtr
listToStrVector xs = do
    vec <- igraphStrvectorNew n
    forM_ (zip [0..] xs) $ \(i,x) -> B.useAsCString x (igraphStrvectorSet vec i)
    return vec
  where
    n = length xs


{#pointer *igraph_matrix_t as MatrixPtr foreign finalizer igraph_matrix_destroy newtype#}

{#fun igraph_matrix_init as igraphMatrixNew { +, `Int', `Int' } -> `MatrixPtr' #}

{#fun igraph_matrix_null as ^ { `MatrixPtr' } -> `()' #}

{#fun igraph_matrix_fill as ^ { `MatrixPtr', `Double' } -> `()' #}

{#fun igraph_matrix_e as ^ { `MatrixPtr', `Int', `Int' } -> `Double' #}

{#fun igraph_matrix_set as ^ { `MatrixPtr', `Int', `Int', `Double' } -> `()' #}

{#fun igraph_matrix_copy_to as ^ { `MatrixPtr', id `Ptr CDouble' } -> `()' #}

{#fun igraph_matrix_nrow as ^ { `MatrixPtr' } -> `Int' #}

{#fun igraph_matrix_ncol as ^ { `MatrixPtr' } -> `Int' #}

listsToMatrixPtr :: [[Double]] -> IO MatrixPtr
listsToMatrixPtr xs = do
    mptr <- igraphMatrixNew r c
    forM_ (zip [0..] xs) $ \(i, row) ->
        forM_ (zip [0..] row) $ \(j,v) ->
            igraphMatrixSet mptr i j v
    return mptr
  where
    r = length xs
    c = maximum $ map length xs

matrixPtrToLists :: MatrixPtr -> IO [[Double]]
matrixPtrToLists mptr = do
    r <- igraphMatrixNrow mptr
    c <- igraphMatrixNcol mptr
    xs <- allocaArray (r*c) $ \ptr -> do
        igraphMatrixCopyTo mptr ptr
        peekArray (r*c) ptr
    return $ transpose $ chunksOf r $ map realToFrac xs
