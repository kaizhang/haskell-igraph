{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph.Mutable
    ( MGraph(..)
    , MLGraph(..)
    , setEdgeAttr
    , setNodeAttr
    , edgeAttr
    , vertexAttr
    )where

import           Control.Monad                  (when, forM)
import           Control.Monad.Primitive
import qualified Data.ByteString.Char8          as B
import           Data.Serialize                 (Serialize, encode)
import           Foreign
import           Foreign.C.String               (CString, withCString)

import           IGraph.Internal.Attribute
import           IGraph.Internal.Data
import           IGraph.Internal.Graph
import           IGraph.Internal.Initialization
import           IGraph.Internal.Selector
import           IGraph.Types

vertexAttr :: String
vertexAttr = "vertex_attribute"

edgeAttr :: String
edgeAttr = "edge_attribute"

-- | Mutable labeled graph.
newtype MLGraph m d v e = MLGraph IGraph

class MGraph d where
    -- | Create a new graph.
    new :: PrimMonad m => Int -> m (MLGraph (PrimState m) d v e)

    -- | Add nodes to the graph.
    addNodes :: PrimMonad m
             => Int  -- ^ The number of new nodes.
             -> MLGraph(PrimState m) d v e -> m ()
    addNodes n (MLGraph g) = unsafePrimToPrim $ igraphAddVertices g n nullPtr

    -- | Add nodes with labels to the graph.
    addLNodes :: (Serialize v, PrimMonad m)
              => [v]  -- ^ vertices' labels
              -> MLGraph (PrimState m) d v e -> m ()
    addLNodes labels (MLGraph g) = unsafePrimToPrim $ do
        bsvec <- toBSVector $ map encode labels
        withAttr vertexAttr bsvec $ \attr -> do
            vptr <- fromPtrs [castPtr attr]
            withVectorPtr vptr (igraphAddVertices g n . castPtr)
      where
        n = length labels

    -- | Delete nodes from the graph.
    delNodes :: PrimMonad m => [Int] -> MLGraph (PrimState m) d v e -> m ()
    delNodes ns (MLGraph g) = unsafePrimToPrim $ do
        vptr <- fromList $ map fromIntegral ns
        vsptr <- igraphVsVector vptr
        igraphDeleteVertices g vsptr
        return ()

    -- | Add edges to the graph.
    addEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()
    addEdges es (MLGraph g) = unsafePrimToPrim $ do
        vec <- fromList xs
        igraphAddEdges g vec nullPtr
      where
        xs = concatMap ( \(a,b) -> [fromIntegral a, fromIntegral b] ) es

    -- | Add edges with labels to the graph.
    addLEdges :: (PrimMonad m, Serialize e) => [LEdge e] -> MLGraph (PrimState m) d v e -> m ()
    addLEdges es (MLGraph g) = unsafePrimToPrim $ do
        bsvec <- toBSVector $ map encode vs
        withAttr edgeAttr bsvec $ \attr -> do
            vec <- fromList $ concat xs
            vptr <- fromPtrs [castPtr attr]
            withVectorPtr vptr (igraphAddEdges g vec . castPtr)
      where
        (xs, vs) = unzip $ map ( \((a,b),v) -> ([fromIntegral a, fromIntegral b], v) ) es

    -- | Delete edges from the graph.
    delEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()

instance MGraph U where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n False >>= return . MLGraph

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        eids <- forM es $ \(fr, to) -> igraphGetEid g fr to False True
        vptr <- fromList $ map fromIntegral eids
        esptr <- igraphEsVector vptr
        igraphDeleteEdges g esptr
        return ()

instance MGraph D where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n True >>= return . MLGraph

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        eids <- forM es $ \(fr, to) -> igraphGetEid g fr to True True
        vptr <- fromList $ map fromIntegral eids
        esptr <- igraphEsVector vptr
        igraphDeleteEdges g esptr
        return ()

-- | Set node attribute.
setNodeAttr :: (PrimMonad m, Serialize v)
            => Int   -- ^ Node id
            -> v
            -> MLGraph (PrimState m) d v e
            -> m ()
setNodeAttr nodeId x (MLGraph gr) = unsafePrimToPrim $ asBS (encode x) $ \bs -> do
    err <- igraphHaskellAttributeVASSet gr vertexAttr nodeId bs
    when (err /= 0) $ error "Fail to set node attribute!"

-- | Set edge attribute.
setEdgeAttr :: (PrimMonad m, Serialize e)
            => Int   -- ^ Edge id
            -> e
            -> MLGraph (PrimState m) d v e
            -> m ()
setEdgeAttr edgeId x (MLGraph gr) = unsafePrimToPrim $ asBS (encode x) $ \bs -> do
    err <- igraphHaskellAttributeEASSet gr edgeAttr edgeId bs
    when (err /= 0) $ error "Fail to set edge attribute!"
