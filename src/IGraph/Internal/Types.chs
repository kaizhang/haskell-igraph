{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Types
    ( -- * Vector type and basic operations
      Vector(..)
    , withVector
    , allocaVector
    , addVectorFinalizer

    -- * Pointer vector
    , VectorPtr(..)
    , withVectorPtr
    , allocaVectorPtr
    , addVectorPtrFinalizer

    -- * String vector
    , StrVector(..)
    , withStrVector
    , allocaStrVector
    , addStrVectorFinalizer

    -- * Bytestring
    , BSLen(..)

    -- * Bytestring vector
    , BSVector(..)
    , withBSVector
    , allocaBSVector
    , addBSVectorFinalizer

    -- * Igraph matrix type
    , Matrix(..)
    , withMatrix
    , allocaMatrix
    , addMatrixFinalizer

    -- * Igraph vertex selector
    , IGraphVs(..)
    , withIGraphVs
    , allocaVs
    , addVsFinalizer

    -- * Igraph vertex iterator
    , IGraphVit(..)
    , withIGraphVit
    , allocaVit
    , addVitFinalizer

    -- * Igraph edge Selector
    , IGraphEs
    , withIGraphEs
    , allocaEs
    , addEsFinalizer

    -- * Igraph edge iterator
    , IGraphEit(..)
    , withIGraphEit
    , allocaEit
    , addEitFinalizer

    -- * IGraph type and basic operations
    , IGraph(..)
    , withIGraph
    , allocaIGraph
    , addIGraphFinalizer

    -- * Igraph attribute record
    , AttributeRecord(..)

    -- * Igraph arpack options type
    , ArpackOpt(..)
    , withArpackOpt
    , igraphArpackNew
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Serialize (Serialize, decode, encode)

import Foreign
import Foreign.C.Types
import Foreign.C.String

{#import IGraph.Internal.Constants #}

#include "haskell_attributes.h"
#include "haskell_igraph.h"

--------------------------------------------------------------------------------
-- Igraph vector
--------------------------------------------------------------------------------

{#pointer *igraph_vector_t as Vector foreign finalizer
    igraph_vector_destroy newtype#}

-- Construtors and destructors

allocaVector :: (Ptr Vector -> IO a) -> IO a
allocaVector f = mallocBytes {# sizeof igraph_vector_t #} >>= f
{-# INLINE allocaVector #-}

addVectorFinalizer :: Ptr Vector -> IO Vector
addVectorFinalizer ptr = do
    vec <- newForeignPtr igraph_vector_destroy ptr
    return $ Vector vec
{-# INLINE addVectorFinalizer #-}


{#pointer *igraph_vector_ptr_t as VectorPtr foreign finalizer
    igraph_vector_ptr_destroy newtype#}

allocaVectorPtr :: (Ptr VectorPtr -> IO a) -> IO a
allocaVectorPtr f = mallocBytes {# sizeof igraph_vector_ptr_t #} >>= f
{-# INLINE allocaVectorPtr #-}

addVectorPtrFinalizer :: Ptr VectorPtr -> IO VectorPtr
addVectorPtrFinalizer ptr = do
    vec <- newForeignPtr igraph_vector_ptr_destroy ptr
    return $ VectorPtr vec
{-# INLINE addVectorPtrFinalizer #-}

--------------------------------------------------------------------------------
-- Igraph string vector
--------------------------------------------------------------------------------

{#pointer *igraph_strvector_t as StrVector foreign finalizer igraph_strvector_destroy newtype#}

allocaStrVector :: (Ptr StrVector -> IO a) -> IO a
allocaStrVector f = mallocBytes {# sizeof igraph_strvector_t #} >>= f
{-# INLINE allocaStrVector #-}

addStrVectorFinalizer :: Ptr StrVector -> IO StrVector
addStrVectorFinalizer ptr = do
    vec <- newForeignPtr igraph_strvector_destroy ptr
    return $ StrVector vec
{-# INLINE addStrVectorFinalizer #-}


--------------------------------------------------------------------------------
-- Customized string vector
--------------------------------------------------------------------------------

{#pointer *bytestring_t as BSLen foreign newtype#}

{#pointer *bsvector_t as BSVector foreign finalizer bsvector_destroy newtype#}

allocaBSVector :: (Ptr BSVector -> IO a) -> IO a
allocaBSVector f = mallocBytes {# sizeof bsvector_t #} >>= f
{-# INLINE allocaBSVector #-}

addBSVectorFinalizer :: Ptr BSVector -> IO BSVector
addBSVectorFinalizer ptr = do
    vec <- newForeignPtr bsvector_destroy ptr
    return $ BSVector vec
{-# INLINE addBSVectorFinalizer #-}

{#pointer *igraph_matrix_t as Matrix foreign finalizer igraph_matrix_destroy newtype#}

allocaMatrix :: (Ptr Matrix -> IO a) -> IO a
allocaMatrix f = mallocBytes {# sizeof igraph_matrix_t #} >>= f
{-# INLINE allocaMatrix #-}

addMatrixFinalizer :: Ptr Matrix -> IO Matrix
addMatrixFinalizer ptr = do
    vec <- newForeignPtr igraph_matrix_destroy ptr
    return $ Matrix vec
{-# INLINE addMatrixFinalizer #-}


{#pointer *igraph_vs_t as IGraphVs foreign finalizer igraph_vs_destroy newtype #}

allocaVs :: (Ptr IGraphVs -> IO a) -> IO a
allocaVs f = mallocBytes {# sizeof igraph_vs_t #} >>= f
{-# INLINE allocaVs #-}

addVsFinalizer :: Ptr IGraphVs -> IO IGraphVs
addVsFinalizer ptr = newForeignPtr igraph_vs_destroy ptr >>= return . IGraphVs
{-# INLINE addVsFinalizer #-}


-- Vertex iterator
{#pointer *igraph_vit_t as IGraphVit foreign finalizer igraph_vit_destroy newtype #}

allocaVit :: (Ptr IGraphVit -> IO a) -> IO a
allocaVit f = mallocBytes {# sizeof igraph_vit_t #} >>= f
{-# INLINE allocaVit #-}

addVitFinalizer :: Ptr IGraphVit -> IO IGraphVit
addVitFinalizer ptr = newForeignPtr igraph_vit_destroy ptr >>= return . IGraphVit
{-# INLINE addVitFinalizer #-}

-- Edge Selector

{#pointer *igraph_es_t as IGraphEs foreign finalizer igraph_es_destroy newtype #}

allocaEs :: (Ptr IGraphEs -> IO a) -> IO a
allocaEs f = mallocBytes {# sizeof igraph_es_t #} >>= f
{-# INLINE allocaEs #-}

addEsFinalizer :: Ptr IGraphEs -> IO IGraphEs
addEsFinalizer ptr = newForeignPtr igraph_es_destroy ptr >>= return . IGraphEs
{-# INLINE addEsFinalizer #-}

-- Edge iterator

{#pointer *igraph_eit_t as IGraphEit foreign finalizer igraph_eit_destroy newtype #}

allocaEit :: (Ptr IGraphEit -> IO a) -> IO a
allocaEit f = mallocBytes {# sizeof igraph_eit_t #} >>= f
{-# INLINE allocaEit #-}

addEitFinalizer :: Ptr IGraphEit -> IO IGraphEit
addEitFinalizer ptr = newForeignPtr igraph_eit_destroy ptr >>= return . IGraphEit
{-# INLINE addEitFinalizer #-}


--------------------------------------------------------------------------------
-- Graph Constructors and Destructors
--------------------------------------------------------------------------------

{#pointer *igraph_t as IGraph foreign finalizer igraph_destroy newtype#}

allocaIGraph :: (Ptr IGraph -> IO a) -> IO a
allocaIGraph f = mallocBytes {# sizeof igraph_t #} >>= f
{-# INLINE allocaIGraph #-}

addIGraphFinalizer :: Ptr IGraph -> IO IGraph
addIGraphFinalizer ptr = do
    vec <- newForeignPtr igraph_destroy ptr
    return $ IGraph vec
{-# INLINE addIGraphFinalizer #-}

{#pointer *igraph_attribute_record_t as AttributeRecord foreign newtype#}

{#pointer *igraph_arpack_options_t as ArpackOpt foreign newtype#}

{#fun igraph_arpack_options_init as igraphArpackNew
    { + } -> `ArpackOpt' #}
