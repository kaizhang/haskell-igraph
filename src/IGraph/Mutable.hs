{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph.Mutable
    ( MGraph(..)
    , MLGraph(..)
    , setEdgeAttr
    , setNodeAttr
    )where

import           Control.Monad                  (when, forM)
import           Control.Monad.Primitive
import           Data.Serialize                 (Serialize, encode)
import           Foreign

import           IGraph.Internal
import           IGraph.Internal.Initialization
import           IGraph.Types

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
    addLNodes labels (MLGraph g) = unsafePrimToPrim $
        withAttr vertexAttr labels $ \attr ->
            withPtrs [attr] (igraphAddVertices g n . castPtr)
      where
        n = length labels

    -- | Delete nodes from the graph.
    delNodes :: PrimMonad m => [Int] -> MLGraph (PrimState m) d v e -> m ()
    delNodes ns (MLGraph g) = unsafePrimToPrim $ withVerticesList ns $ \vs ->
        igraphDeleteVertices g vs

    -- | Add edges to the graph.
    addEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()
    addEdges es (MLGraph g) = unsafePrimToPrim $ withList xs $ \vec ->
        igraphAddEdges g vec nullPtr
      where
        xs = concatMap ( \(a,b) -> [a, b] ) es

    -- | Add edges with labels to the graph.
    addLEdges :: (PrimMonad m, Serialize e) => [LEdge e] -> MLGraph (PrimState m) d v e -> m ()
    addLEdges es (MLGraph g) = unsafePrimToPrim $
        withAttr edgeAttr vs $ \attr -> withList (concat xs) $ \vec ->
            withPtrs [attr] (igraphAddEdges g vec . castPtr)
      where
        (xs, vs) = unzip $ map ( \((a,b),v) -> ([a, b], v) ) es

    -- | Delete edges from the graph.
    delEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()

instance MGraph U where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n False >>= return . MLGraph

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        eids <- forM es $ \(fr, to) -> igraphGetEid g fr to False True
        withEdgesList eids (igraphDeleteEdges g)

instance MGraph D where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n True >>= return . MLGraph

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        eids <- forM es $ \(fr, to) -> igraphGetEid g fr to True True
        withEdgesList eids (igraphDeleteEdges g)

-- | Set node attribute.
setNodeAttr :: (PrimMonad m, Serialize v)
            => Int   -- ^ Node id
            -> v
            -> MLGraph (PrimState m) d v e
            -> m ()
setNodeAttr nodeId x (MLGraph gr) = unsafePrimToPrim $
    withByteString (encode x) $ \bs -> do
        err <- igraphHaskellAttributeVASSet gr vertexAttr nodeId bs
        when (err /= 0) $ error "Fail to set node attribute!"

-- | Set edge attribute.
setEdgeAttr :: (PrimMonad m, Serialize e)
            => Int   -- ^ Edge id
            -> e
            -> MLGraph (PrimState m) d v e
            -> m ()
setEdgeAttr edgeId x (MLGraph gr) = unsafePrimToPrim $
    withByteString (encode x) $ \bs -> do
        err <- igraphHaskellAttributeEASSet gr edgeAttr edgeId bs
        when (err /= 0) $ error "Fail to set edge attribute!"
