{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal
    ( -- * Data structure library: vector, matrix, other data types
      -- ** Igraph vector type and basic operations
      Vector
    , allocaVector
    , allocaVectorN
    , withList
    , withListMaybe
    , toList
    , igraphVectorNull
    , igraphVectorFill
    , igraphVectorE
    , igraphVectorSet
    , igraphVectorTail
    , igraphVectorSize
    , igraphVectorCopyTo

    -- ** Igraph pointer vector
    , VectorPtr
    , allocaVectorPtr
    , allocaVectorPtrN
    , withPtrs
    , toLists

      -- ** Customized bytestring for storing attributes
    , BSLen
    , withByteString
    , toByteString

      -- ** Customized bytestring vector
    , BSVector
    , allocaBSVectorN
    , withByteStrings
    , bsvectorSet

      -- ** Igraph matrix type
    , Matrix
    , allocaMatrix
    , allocaMatrixN
    , withRowLists
    , toRowLists
    , toColumnLists
    , igraphMatrixNull
    , igraphMatrixFill
    , igraphMatrixE
    , igraphMatrixSet
    , igraphMatrixCopyTo
    , igraphMatrixNrow
    , igraphMatrixNcol

      -- * Igraph type and constructors
    , IGraph
    , withIGraph
    , allocaIGraph
    , addIGraphFinalizer
    , igraphNew

      -- * Selector and iterator for edge and vertex
      -- ** Igraph vertex selector
    , VertexSelector
    , withVerticesAll
    , withVerticesAdj
    , withVerticesVector
    , withVerticesList

      -- ** Igraph vertex iterator
    , VertexIterator
    , iterateVertices
    , iterateVerticesC

      -- ** Igraph edge Selector
    , EdgeSelector
    , withEdgesAll
    , withEdgesVector
    , withEdgesList

      -- ** Igraph edge iterator
    , EdgeIterator
    , iterateEdges
    , iterateEdgesC

      -- * Basic graph operations
    , igraphCopy
    , igraphVcount
    , igraphEcount
    , igraphGetEid
    , igraphEdge
    , igraphAddVertices
    , igraphAddEdge
    , igraphAddEdges
    , igraphDeleteVertices
    , igraphDeleteEdges

      -- * Igraph attribute record
    , AttributeRecord
    , withAttr
    , withBSAttr
    , igraphHaskellAttributeHasAttr
    , igraphHaskellAttributeGANSet
    , igraphHaskellAttributeGAN
    , igraphHaskellAttributeVAS
    , igraphHaskellAttributeEAN
    , igraphHaskellAttributeEAS
    , igraphHaskellAttributeEASSetv
    , igraphHaskellAttributeVASSet
    , igraphHaskellAttributeEASSet

      -- * Igraph arpack options type
    , ArpackOpt
    , allocaArpackOpt
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Serialize (Serialize, encode)
import Control.Exception (bracket_)
import Conduit (ConduitT, yield, liftIO)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import IGraph.Internal.C2HS

{#import IGraph.Internal.Initialization #}
{#import IGraph.Internal.Constants #}

#include "haskell_attributes.h"
#include "haskell_igraph.h"

--------------------------------------------------------------------------------
-- Igraph vector
--------------------------------------------------------------------------------

data Vector

-- | Allocate and initialize a vector.
allocaVector :: (Ptr Vector -> IO a) -> IO a
allocaVector fun = allocaBytes {# sizeof igraph_vector_t #} $ \vec ->
    bracket_ (igraphVectorInit vec 0) (igraphVectorDestroy vec) (fun vec)
{-# INLINE allocaVector #-}

allocaVectorN :: Int -> (Ptr Vector -> IO a) -> IO a
allocaVectorN n fun = allocaBytes {# sizeof igraph_vector_t #} $ \vec ->
    bracket_ (igraphVectorInit vec n) (igraphVectorDestroy vec) (fun vec)
{-# INLINE allocaVectorN #-}

{#fun igraph_vector_init as ^ { castPtr `Ptr Vector', `Int' } -> `CInt' void- #}
{#fun igraph_vector_destroy as ^ { castPtr `Ptr Vector' } -> `CInt' void- #}

withList :: Real a => [a] -> (Ptr Vector -> IO b) -> IO b
withList xs fun = withArrayLen (map realToFrac xs) $ \n ptr ->
    allocaBytes {# sizeof igraph_vector_t #} $ \vec ->
        bracket_ (igraphVectorInitCopy vec ptr n) (igraphVectorDestroy vec) (fun vec)
{-# INLINE withList #-}
{#fun igraph_vector_init_copy as ^
    { castPtr `Ptr Vector'
    , id `Ptr CDouble', `Int' } -> `CInt' void- #}

-- | Allocate a nullPtr if Nothing
withListMaybe :: Real a => Maybe [a] -> (Ptr Vector -> IO b) -> IO b
withListMaybe (Just xs) fun = withList xs fun
withListMaybe Nothing fun = fun $ castPtr nullPtr
{-# INLINE withListMaybe #-}


toList :: Ptr Vector -> IO [Double]
toList vec = do
    n <- igraphVectorSize vec
    allocaArray n $ \ptr -> do
        igraphVectorCopyTo vec ptr
        liftM (map realToFrac) $ peekArray n ptr
{-# INLINE toList #-}

{#fun igraph_vector_copy_to as ^ { castPtr `Ptr Vector', id `Ptr CDouble' } -> `()' #}

-- Initializing elements

{#fun igraph_vector_null as ^ { castPtr `Ptr Vector' } -> `()' #}

{#fun igraph_vector_fill as ^ { castPtr `Ptr Vector', `Double' } -> `()' #}


-- Accessing elements

{#fun igraph_vector_e as ^ { castPtr `Ptr Vector', `Int' } -> `Double' #}

{#fun igraph_vector_set as ^ { castPtr `Ptr Vector', `Int', `Double' } -> `()' #}

{#fun igraph_vector_tail as ^ { castPtr `Ptr Vector' } -> `Double' #}


-- Vector properties
{#fun igraph_vector_size as ^ { castPtr `Ptr Vector' } -> `Int' #}


--------------------------------------------------------------------------------
-- Pointer Vector
--------------------------------------------------------------------------------

data VectorPtr

-- | Allocate and initialize a pointer vector.
allocaVectorPtr :: (Ptr VectorPtr -> IO a) -> IO a
allocaVectorPtr fun = allocaBytes {# sizeof igraph_vector_ptr_t #} $ \ptr ->
    bracket_ (igraphVectorPtrInit ptr 0) (igraphVectorPtrDestroy ptr) (fun ptr)
{-# INLINE allocaVectorPtr #-}

allocaVectorPtrN :: Int -> (Ptr VectorPtr -> IO a) -> IO a
allocaVectorPtrN n fun = allocaBytes {# sizeof igraph_vector_ptr_t #} $ \ptr ->
    bracket_ (igraphVectorPtrInit ptr n) (igraphVectorPtrDestroy ptr) (fun ptr)
{-# INLINE allocaVectorPtrN #-}

{#fun igraph_vector_ptr_init as ^ { castPtr `Ptr VectorPtr', `Int' } -> `CInt' void- #}
{#fun igraph_vector_ptr_destroy as ^ { castPtr `Ptr VectorPtr' } -> `()' #}

withPtrs :: [Ptr a] -> (Ptr VectorPtr -> IO b) -> IO b
withPtrs xs fun = allocaVectorPtrN n $ \vptr -> do
    sequence_ $ zipWith (igraphVectorPtrSet vptr) [0..] $ map castPtr xs
    fun vptr
  where
    n = length xs
{-# INLINE withPtrs #-}

toLists :: Ptr VectorPtr -> IO [[Double]]
toLists vptr = do
    n <- igraphVectorPtrSize vptr
    forM [0..n-1] $ \i -> igraphVectorPtrE vptr i >>= toList . castPtr
{-# INLINE toLists #-}

{#fun igraph_vector_ptr_e as ^ { castPtr `Ptr VectorPtr', `Int' } -> `Ptr ()' #}
{#fun igraph_vector_ptr_set as ^ { castPtr `Ptr VectorPtr', `Int', id `Ptr ()' } -> `()' #}
{#fun igraph_vector_ptr_size as ^ { castPtr `Ptr VectorPtr' } -> `Int' #}


--------------------------------------------------------------------------------
-- Customized string vector
--------------------------------------------------------------------------------

data BSLen

toByteString :: Ptr BSLen -> IO B.ByteString
toByteString ptr = do
    n <- {#get bytestring_t->len #} ptr
    str <- {#get bytestring_t->value #} ptr
    packCStringLen (str, fromIntegral n)
{-# INLINE toByteString #-}

withByteString :: B.ByteString -> (Ptr BSLen -> IO a) -> IO a
withByteString x f = unsafeUseAsCStringLen x $ \(str, n) ->
    allocaBytes {#sizeof bytestring_t #} $ \ptr -> do
        {#set bytestring_t.len #} ptr (fromIntegral n)
        {#set bytestring_t.value #} ptr str
        f ptr
{-# INLINE withByteString #-}

data BSVector

allocaBSVectorN :: Int -> (Ptr BSVector -> IO a) -> IO a
allocaBSVectorN n fun = allocaBytes {# sizeof bsvector_t #} $ \ptr ->
    bracket_ (bsvectorInit ptr n) (bsvectorDestroy ptr) (fun ptr)
{-# INLINE allocaBSVectorN #-}

{#fun bsvector_init as ^ { castPtr `Ptr BSVector', `Int' } -> `CInt' void- #}
{#fun bsvector_destroy as ^ { castPtr `Ptr BSVector' } -> `()' #}

withByteStrings :: [B.ByteString] -> (Ptr BSVector -> IO a) -> IO a
withByteStrings xs fun = allocaBSVectorN n $ \bsvec -> do
    foldM_ (\i x -> bsvectorSet bsvec i x >> return (i+1)) 0 xs
    fun bsvec
  where
    n = length xs
{-# INLINE withByteStrings #-}

bsvectorSet :: Ptr BSVector -> Int -> B.ByteString -> IO ()
bsvectorSet vec i bs = withByteString bs (bsvectorSet' vec i)
{-# INLINE bsvectorSet #-}
{#fun bsvector_set as bsvectorSet'
    { castPtr `Ptr BSVector', `Int', castPtr `Ptr BSLen' } -> `()' #}


--------------------------------------------------------------------------------
-- Matrix
--------------------------------------------------------------------------------

data Matrix

allocaMatrix :: (Ptr Matrix -> IO a) -> IO a
allocaMatrix fun = allocaBytes {# sizeof igraph_matrix_t #} $ \mat ->
    bracket_ (igraphMatrixInit mat 0 0) (igraphMatrixDestroy mat) (fun mat)
{-# INLINE allocaMatrix #-}

allocaMatrixN :: Int   -- ^ Number of rows
              -> Int   -- ^ Number of columns
              -> (Ptr Matrix -> IO a) -> IO a
allocaMatrixN r c fun = allocaBytes {# sizeof igraph_matrix_t #} $ \mat ->
    bracket_ (igraphMatrixInit mat r c) (igraphMatrixDestroy mat) (fun mat)
{-# INLINE allocaMatrixN #-}

{#fun igraph_matrix_init as ^ { castPtr `Ptr Matrix', `Int', `Int' } -> `CInt' void- #}
{#fun igraph_matrix_destroy as ^ { castPtr `Ptr Matrix' } -> `()' #}

-- row lists to matrix
withRowLists :: Real a => [[a]] -> (Ptr Matrix -> IO b) -> IO b
withRowLists xs fun
    | all (==c) $ map length xs = allocaMatrixN r c $ \mat -> do
        forM_ (zip [0..] xs) $ \(i, row) ->
            forM_ (zip [0..] row) $ \(j,v) ->
                igraphMatrixSet mat i j $ realToFrac v
        fun mat
    | otherwise = error "Not a matrix."
  where
    r = length xs
    c = length $ head xs
{-# INLINE withRowLists #-}

-- to row lists
toRowLists :: Ptr Matrix -> IO [[Double]]
toRowLists = fmap transpose . toColumnLists

toColumnLists :: Ptr Matrix -> IO [[Double]]
toColumnLists mptr = do
    r <- igraphMatrixNrow mptr
    c <- igraphMatrixNcol mptr
    xs <- allocaArray (r*c) $ \ptr -> do
        igraphMatrixCopyTo mptr ptr
        peekArray (r*c) ptr
    return $ chunksOf r $ map realToFrac xs

{#fun igraph_matrix_null as ^ { castPtr `Ptr Matrix' } -> `()' #}

{#fun igraph_matrix_fill as ^ { castPtr `Ptr Matrix', `Double' } -> `()' #}

{#fun igraph_matrix_e as ^ { castPtr `Ptr Matrix', `Int', `Int' } -> `Double' #}

{#fun igraph_matrix_set as ^ { castPtr `Ptr Matrix', `Int', `Int', `Double' } -> `()' #}

{#fun igraph_matrix_copy_to as ^ { castPtr `Ptr Matrix', id `Ptr CDouble' } -> `()' #}

{#fun igraph_matrix_nrow as ^ { castPtr `Ptr Matrix' } -> `Int' #}

{#fun igraph_matrix_ncol as ^ { castPtr `Ptr Matrix' } -> `Int' #}


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

{#fun igraph_empty as igraphNew'
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `Int', `Bool'
    } -> `CInt' void- #}

{#fun igraph_copy as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `IGraph'
    } -> `CInt' void- #}

-- | Create a igraph object and attach a finalizer
igraphNew :: Int -> Bool -> HasInit -> IO IGraph
igraphNew n directed _ = igraphNew' n directed


--------------------------------------------------------------------------------
-- Vertex selector
--------------------------------------------------------------------------------

data VertexSelector

allocaVertexSelector :: (Ptr VertexSelector -> IO a) -> IO a
allocaVertexSelector fun = allocaBytes {# sizeof igraph_vs_t #} $ \vs -> do
    r <- fun vs
    igraphVsDestroy vs
    return r
{-# INLINE allocaVertexSelector #-}

{#fun igraph_vs_destroy as ^ { castPtr `Ptr VertexSelector' } -> `()' #}

withVerticesAll :: (Ptr VertexSelector -> IO a) -> IO a
withVerticesAll fun = allocaVertexSelector $ \vs -> igraphVsAll vs >> fun vs
{-# INLINE withVerticesAll #-}
{#fun igraph_vs_all as ^ { castPtr `Ptr VertexSelector' } -> `CInt' void- #}

withVerticesAdj :: Int -> Neimode -> (Ptr VertexSelector -> IO a) -> IO a
withVerticesAdj i mode fun = allocaVertexSelector $ \vs -> igraphVsAdj vs i mode >> fun vs
{-# INLINE withVerticesAdj #-}
{#fun igraph_vs_adj as ^
    { castPtr `Ptr VertexSelector', `Int', `Neimode' } -> `CInt' void- #}

withVerticesVector :: Ptr Vector -> (Ptr VertexSelector -> IO a) -> IO a
withVerticesVector vec fun = allocaVertexSelector $ \vs -> igraphVsVector vs vec >> fun vs
{-# INLINE withVerticesVector #-}
{#fun igraph_vs_vector as ^
    { castPtr `Ptr VertexSelector', castPtr `Ptr Vector' } -> `CInt' void- #}

withVerticesList :: Real a => [a] -> (Ptr VertexSelector -> IO b) -> IO b
withVerticesList xs fun = withList xs $ \vec -> withVerticesVector vec fun
{-# INLINE withVerticesList #-}


--------------------------------------------------------------------------------
-- Vertex iterator
--------------------------------------------------------------------------------

data VertexIterator

iterateVertices :: IGraph -> Ptr VertexSelector -> (Ptr VertexIterator -> IO a) -> IO a
iterateVertices gr vs fun = allocaBytes {# sizeof igraph_vit_t #} $ \vit ->
    bracket_ (igraphVitCreate gr vs vit) (igraphVitDestroy vit) (fun vit)
{-# INLINE iterateVertices #-}

iterateVerticesC :: IGraph
                 -> Ptr VertexSelector
                 -> (ConduitT i Int IO () -> IO a)
                 -> IO a
iterateVerticesC gr vs fun = allocaBytes {# sizeof igraph_vit_t #} $ \vit ->
    bracket_ (igraphVitCreate gr vs vit) (igraphVitDestroy vit) (fun $ sourceVertexIterator vit)
{-# INLINE iterateVerticesC #-}

{#fun igraph_vit_create as ^
    { `IGraph'
    , castPtr %`Ptr VertexSelector'
    , castPtr `Ptr VertexIterator'
    } -> `CInt' void- #}
{#fun igraph_vit_destroy as ^ { castPtr `Ptr VertexIterator' } -> `()' #}


sourceVertexIterator :: Ptr VertexIterator -> ConduitT i Int IO ()
sourceVertexIterator vit = do
    isEnd <- liftIO $ igraphVitEnd vit
    if isEnd
      then return ()
      else do
        liftIO (igraphVitGet vit) >>= yield
        liftIO $ igraphVitNext vit
        sourceVertexIterator vit
{-# INLINE sourceVertexIterator #-}

#c
igraph_bool_t igraph_vit_end(igraph_vit_t *vit) {
  return IGRAPH_VIT_END(*vit);
}
void igraph_vit_next(igraph_vit_t *vit) {
  IGRAPH_VIT_NEXT(*vit);
}
igraph_integer_t igraph_vit_get(igraph_vit_t *vit) {
  return IGRAPH_VIT_GET(*vit);
}
#endc

{#fun igraph_vit_end as ^ { castPtr `Ptr VertexIterator' } -> `Bool' #}
{#fun igraph_vit_next as ^ { castPtr `Ptr VertexIterator' } -> `()' #}
{#fun igraph_vit_get as ^ { castPtr `Ptr VertexIterator' } -> `Int' #}


--------------------------------------------------------------------------------
-- Edge Selector
--------------------------------------------------------------------------------

data EdgeSelector

allocaEdgeSelector :: (Ptr EdgeSelector -> IO a) -> IO a
allocaEdgeSelector fun = allocaBytes {# sizeof igraph_es_t #} $ \es -> do
    r <- fun es
    igraphEsDestroy es
    return r
{-# INLINE allocaEdgeSelector #-}
{#fun igraph_es_destroy as ^ { castPtr `Ptr EdgeSelector' } -> `()' #}

withEdgesAll :: EdgeOrderType -> (Ptr EdgeSelector -> IO a) -> IO a
withEdgesAll ord fun = allocaEdgeSelector $ \es -> igraphEsAll es ord >> fun es
{-# INLINE withEdgesAll #-}
{#fun igraph_es_all as ^ { castPtr `Ptr EdgeSelector', `EdgeOrderType'} -> `CInt' void- #}

withEdgesVector :: Ptr Vector -> (Ptr EdgeSelector -> IO a) -> IO a
withEdgesVector vec fun = allocaEdgeSelector $ \es ->
    igraphEsVector es vec >> fun es
{-# INLINE withEdgesVector #-}
{# fun igraph_es_vector as ^
    { castPtr `Ptr EdgeSelector', castPtr `Ptr Vector' } -> `CInt' void- #}

withEdgesList :: Real a => [a] -> (Ptr EdgeSelector -> IO b) -> IO b
withEdgesList xs fun = withList xs $ \vec -> withEdgesVector vec fun
{-# INLINE withEdgesList #-}


--------------------------------------------------------------------------------
-- Edge iterator
--------------------------------------------------------------------------------

data EdgeIterator

iterateEdges :: IGraph -> Ptr EdgeSelector -> (Ptr EdgeIterator -> IO a) -> IO a
iterateEdges gr es fun = allocaBytes {# sizeof igraph_eit_t #} $ \eit ->
    bracket_ (igraphEitCreate gr es eit) (igraphEitDestroy eit) (fun eit)
{-# INLINE iterateEdges #-}
{#fun igraph_eit_create as ^ { `IGraph', castPtr %`Ptr EdgeSelector', castPtr `Ptr EdgeIterator' } -> `CInt' void- #}
{#fun igraph_eit_destroy as ^ { castPtr `Ptr EdgeIterator' } -> `()' #}

iterateEdgesC :: IGraph
              -> Ptr EdgeSelector
              -> (ConduitT i Int IO () -> IO a)
              -> IO a
iterateEdgesC gr es fun = allocaBytes {# sizeof igraph_eit_t #} $ \eit ->
    bracket_ (igraphEitCreate gr es eit) (igraphEitDestroy eit) (fun $ sourceEdgeIterator eit)
{-# INLINE iterateEdgesC #-}

sourceEdgeIterator :: Ptr EdgeIterator -> ConduitT i Int IO ()
sourceEdgeIterator eit = do
    isEnd <- liftIO $ igraphEitEnd eit
    if isEnd
      then return ()
      else do
        liftIO (igraphEitGet eit) >>= yield
        liftIO $ igraphEitNext eit
        sourceEdgeIterator eit
{-# INLINE sourceEdgeIterator #-}

#c
igraph_bool_t igraph_eit_end(igraph_eit_t *eit) {
  return IGRAPH_EIT_END(*eit);
}
void igraph_eit_next(igraph_eit_t *eit) {
  IGRAPH_EIT_NEXT(*eit);
}
igraph_integer_t igraph_eit_get(igraph_eit_t *eit) {
  return IGRAPH_EIT_GET(*eit);
}
#endc
{#fun igraph_eit_end as ^ { castPtr `Ptr EdgeIterator' } -> `Bool' #}
{#fun igraph_eit_next as ^ { castPtr `Ptr EdgeIterator' } -> `()' #}
{#fun igraph_eit_get as ^ { castPtr `Ptr EdgeIterator' } -> `Int' #}


--------------------------------------------------------------------------------
-- Basic Query Operations
--------------------------------------------------------------------------------

{#fun igraph_vcount as ^ { `IGraph' } -> `Int' #}

{#fun igraph_ecount as ^ { `IGraph' } -> `Int' #}

{#fun igraph_get_eid as ^
    { `IGraph'
    , alloca- `Int' peekIntConv*
    , `Int'
    , `Int'
    , `Bool'
    , `Bool'
    } -> `CInt' void-#}

{#fun igraph_edge as ^
    { `IGraph'
    , `Int'
    , alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    } -> `CInt' void-#}

-- Adding and Deleting Vertices and Edges

{# fun igraph_add_vertices as ^ { `IGraph', `Int', id `Ptr ()' } -> `()' #}

{# fun igraph_add_edge as ^ { `IGraph', `Int', `Int' } -> `()' #}

-- | The edges are given in a vector, the first two elements define the first
-- edge (the order is from , to for directed graphs). The vector should
-- contain even number of integer numbers between zero and the number of
-- vertices in the graph minus one (inclusive). If you also want to add
-- new vertices, call igraph_add_vertices() first.
{# fun igraph_add_edges as ^
    { `IGraph'     -- ^ The graph to which the edges will be added.
    , castPtr `Ptr Vector'     -- ^ The edges themselves.
    , id `Ptr ()'  -- ^ The attributes of the new edges.
    } -> `()' #}

-- | delete vertices
{# fun igraph_delete_vertices as ^
    { `IGraph', castPtr %`Ptr VertexSelector' } -> `CInt' void- #}

-- | delete edges
{# fun igraph_delete_edges as ^
    { `IGraph', castPtr %`Ptr EdgeSelector' } -> `CInt' void- #}

data AttributeRecord

withAttr :: Serialize a
         => String   -- ^ Attribute name
         -> [a]      -- ^ Attributes
         -> (Ptr AttributeRecord -> IO b) -> IO b
withAttr name xs fun = withByteStrings (map encode xs) $ \bsvec ->
    withBSAttr name bsvec fun
{-# INLINE withAttr #-}

withBSAttr :: String          -- ^ Attribute name
           -> Ptr BSVector    -- ^ Attributes
           -> (Ptr AttributeRecord -> IO b) -> IO b
withBSAttr name bsvec fun = withCString name $ \name' ->
    allocaBytes {#sizeof igraph_attribute_record_t #} $ \attr ->
        setAttribute attr name' (castPtr bsvec) >> fun attr
  where
    setAttribute attr x y = do
        {#set igraph_attribute_record_t.name #} attr x
        {#set igraph_attribute_record_t.type #} attr 2
        {#set igraph_attribute_record_t.value #} attr y
{-# INLINE withBSAttr #-}

{#fun igraph_haskell_attribute_has_attr as ^
    { `IGraph', `AttributeElemtype', `String' } -> `Bool' #}

{#fun igraph_haskell_attribute_GAN_set as ^
    { `IGraph', `String', `Double' } -> `Int' #}

{#fun igraph_haskell_attribute_GAN as ^
    { `IGraph', `String' } -> `Double' #}

{#fun igraph_haskell_attribute_VAS as ^
    { `IGraph', `String', `Int' } -> `Ptr BSLen' castPtr #}

{#fun igraph_haskell_attribute_EAN as ^
    { `IGraph', `String', `Int' } -> `Double' #}
{#fun igraph_haskell_attribute_EAS as ^
    { `IGraph', `String', `Int' } -> `Ptr BSLen' castPtr #}

{#fun igraph_haskell_attribute_EAS_setv as ^
    { `IGraph', `String', castPtr `Ptr BSVector' } -> `Int' #}

{#fun igraph_haskell_attribute_VAS_set as ^
    { `IGraph', `String', `Int', castPtr `Ptr BSLen' } -> `Int' #}

{#fun igraph_haskell_attribute_EAS_set as ^
    { `IGraph', `String', `Int', castPtr `Ptr BSLen' } -> `Int' #}


--------------------------------------------------------------------------------
-- Arpack options
--------------------------------------------------------------------------------

data ArpackOpt

allocaArpackOpt :: (Ptr ArpackOpt -> IO a) -> IO a
allocaArpackOpt fun = allocaBytes {# sizeof igraph_arpack_options_t #} $ \opt -> do
    igraphArpackOptionsInit opt >> fun opt
{-# INLINE allocaArpackOpt #-}
{#fun igraph_arpack_options_init as ^ { castPtr `Ptr ArpackOpt' } -> `CInt' void- #}
