{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal
    ( module IGraph.Internal.Types
    -- * Vector type and basic operations
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

    -- * Pointer vector
    , igraphVectorPtrNew
    , fromPtrs
    , toLists

    -- * String vector
    , igraphStrvectorNew
    , igraphStrvectorGet
    , toStrVector

    -- * Bytestring
    , asBS
    , bsToByteString

    -- * Bytestring vector
    , bsvectorNew
    , bsvectorSet
    , toBSVector

    -- * Igraph matrix type
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

    -- * Igraph vertex selector
    , igraphVsAll
    , igraphVsAdj
    , igraphVsVector

    -- * Igraph vertex iterator
    , igraphVitNew
    , vitToList

    -- * Igraph edge Selector
    , igraphEsAll
    , igraphEsVector

    -- * Igraph edge iterator
    , igraphEitNew
    , eitToList

    -- * IGraph type and basic operations
    , igraphNew
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
    , withAttr
    , igraphHaskellAttributeHasAttr
    , igraphHaskellAttributeGANSet
    , igraphHaskellAttributeGAN
    , igraphHaskellAttributeVAS
    , igraphHaskellAttributeEAN
    , igraphHaskellAttributeEAS
    , igraphHaskellAttributeEASSetv
    , igraphHaskellAttributeVASSet
    , igraphHaskellAttributeEASSet
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
import IGraph.Internal.C2HS

{#import IGraph.Internal.Initialization #}
{#import IGraph.Internal.Types #}
{#import IGraph.Internal.Constants #}

#include "haskell_attributes.h"
#include "haskell_igraph.h"

--------------------------------------------------------------------------------
-- Igraph vector
--------------------------------------------------------------------------------

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


{#fun igraph_vector_ptr_init as igraphVectorPtrNew
    { allocaVectorPtr- `VectorPtr' addVectorPtrFinalizer*
    , `Int' } -> `CInt' void- #}

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

{#fun igraph_strvector_init as igraphStrvectorNew
    { allocaStrVector- `StrVector' addStrVectorFinalizer*
    , `Int'
    } -> `CInt' void-#}

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


{#fun igraph_matrix_init as igraphMatrixNew
    { allocaMatrix- `Matrix' addMatrixFinalizer*
    , `Int', `Int'
    } -> `CInt' void- #}

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


{#fun igraph_vs_all as ^
    { allocaVs- `IGraphVs' addVsFinalizer*
    } -> `CInt' void- #}

{#fun igraph_vs_adj as ^
    { allocaVs- `IGraphVs' addVsFinalizer*
    , `Int', `Neimode'
    } -> `CInt' void- #}

{#fun igraph_vs_vector as ^
    { allocaVs- `IGraphVs' addVsFinalizer*
    , `Vector'
    } -> `CInt' void- #}

-- Vertex iterator

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

{#fun igraph_vit_create as igraphVitNew
    { `IGraph'
    , %`IGraphVs'
    , allocaVit- `IGraphVit' addVitFinalizer*
    } -> `CInt' void- #}

{#fun igraph_vit_end as ^ { `IGraphVit' } -> `Bool' #}

{#fun igraph_vit_next as ^ { `IGraphVit' } -> `()' #}

{#fun igraph_vit_get as ^ { `IGraphVit' } -> `Int' #}

vitToList :: IGraphVit -> IO [Int]
vitToList vit = do
    isEnd <- igraphVitEnd vit
    if isEnd
      then return []
      else do
        cur <- igraphVitGet vit
        igraphVitNext vit
        acc <- vitToList vit
        return $ cur : acc


-- Edge Selector

{#fun igraph_es_all as ^
    { allocaEs- `IGraphEs' addEsFinalizer*
    , `EdgeOrderType'
    } -> `CInt' void- #}

{# fun igraph_es_vector as ^
    { allocaEs- `IGraphEs' addEsFinalizer*
    , `Vector'
    } -> `CInt' void- #}

-- Edge iterator

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

{#fun igraph_eit_create as igraphEitNew
    { `IGraph'
    , %`IGraphEs'
    , allocaEit- `IGraphEit' addEitFinalizer*
    } -> `CInt' void- #}

{#fun igraph_eit_end as ^ { `IGraphEit' } -> `Bool' #}

{#fun igraph_eit_next as ^ { `IGraphEit' } -> `()' #}

{#fun igraph_eit_get as ^ { `IGraphEit' } -> `Int' #}

eitToList :: IGraphEit -> IO [Int]
eitToList eit = do
    isEnd <- igraphEitEnd eit
    if isEnd
      then return []
      else do
        cur <- igraphEitGet eit
        igraphEitNext eit
        acc <- eitToList eit
        return $ cur : acc


--------------------------------------------------------------------------------
-- Graph Constructors and Destructors
--------------------------------------------------------------------------------

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
    , `Vector'     -- ^ The edges themselves.
    , id `Ptr ()'  -- ^ The attributes of the new edges.
    } -> `()' #}

-- | delete vertices
{# fun igraph_delete_vertices as ^ { `IGraph', %`IGraphVs' } -> `Int' #}

-- | delete edges
{# fun igraph_delete_edges as ^ { `IGraph', %`IGraphEs' } -> `Int' #}



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
