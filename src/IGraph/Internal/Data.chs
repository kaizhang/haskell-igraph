{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Data where

import qualified Data.ByteString.Char8 as B
import Control.Monad (forM_)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

#include "igraph/igraph.h"
#include "cbits/igraph.c"

data Vector
{#pointer *igraph_vector_t as VectorPtr -> Vector #}

-- Construtors and destructors

{#fun igraph_vector_new as ^ { `Int' } -> `VectorPtr' #}

{#fun igraph_vector_destroy as ^ { `VectorPtr' } -> `()' #}

listToVector :: [Double] -> IO VectorPtr
listToVector xs = do
    vec <- igraphVectorNew n
    forM_ (zip [0..] xs) $ \(i,x) -> igraphVectorSet vec i x
    return vec
  where
    n = length xs


-- Initializing elements

{#fun igraph_vector_null as ^ { `VectorPtr' } -> `()' #}

{#fun igraph_vector_fill as ^ { `VectorPtr', `Double' } -> `()' #}


-- Accessing elements

{#fun pure igraph_vector_e as ^ { `VectorPtr', `Int' } -> `Double' #}

{#fun igraph_vector_set as ^ { `VectorPtr', `Int', `Double' } -> `()' #}

{#fun pure igraph_vector_tail as ^ { `VectorPtr' } -> `Double' #}


data VectorP
{#pointer *igraph_vector_ptr_t as VectorPPtr -> VectorP #}

{#fun igraph_vector_ptr_new as ^ { `Int' } -> `VectorPPtr' #}

{#fun igraph_vector_ptr_destroy as ^ { `VectorPPtr' } -> `()' #}
{#fun igraph_vector_ptr_destroy_all as ^ { `VectorPPtr' } -> `()' #}

{#fun igraph_vector_ptr_set as ^ { `VectorPPtr', `Int', id `Ptr ()' } -> `()' #}

listToVectorP :: [Ptr ()] -> IO VectorPPtr
listToVectorP xs = do
    vptr <- igraphVectorPtrNew n
    forM_ (zip [0..] xs) $ \(i,x) -> igraphVectorPtrSet vptr i x
    return vptr
  where
    n = length xs

data StrVector
{#pointer *igraph_strvector_t as StrVectorPtr -> StrVector #}

{#fun igraph_strvector_new as ^ { `Int' } -> `StrVectorPtr' #}

{#fun igraph_strvector_destroy as ^ { `StrVectorPtr' } -> `()' #}

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


data Matrix
{#pointer *igraph_matrix_t as MatrixPtr -> Matrix #}

{#fun igraph_matrix_new as ^ { `Int', `Int' } -> `MatrixPtr' #}

{#fun igraph_matrix_destroy as ^ { `MatrixPtr' } -> `()' #}

{#fun igraph_matrix_null as ^ { `MatrixPtr' } -> `()' #}

{#fun igraph_matrix_fill as ^ { `MatrixPtr', `Double' } -> `()' #}

{#fun pure igraph_matrix_e as ^ { `MatrixPtr', `Int', `Int' } -> `Double' #}

{#fun pure igraph_matrix_set as ^ { `MatrixPtr', `Int', `Int', `Double' } -> `()' #}
