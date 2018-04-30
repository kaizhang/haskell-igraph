{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IGraph.Mutable
    ( MGraph(..)
    , new
    , addNodes
    , addLNodes
    , delNodes
    , addEdges
    , addLEdges
    , delEdges
    , setEdgeAttr
    , setNodeAttr
    )where

import           Control.Monad                  (forM, when)
import           Control.Monad.Primitive
import           Data.Serialize                 (Serialize, encode)
import           Data.Singletons.Prelude        (Sing, SingI, fromSing, sing)
import           Foreign                        hiding (new)

import           IGraph.Internal
import           IGraph.Internal.Initialization
import           IGraph.Types

-- | Mutable labeled graph.
newtype MGraph m (d :: EdgeType) v e = MGraph IGraph

-- | Create a new graph.
new :: forall m d v e. (SingI d, PrimMonad m)
    => Int -> m (MGraph (PrimState m) d v e)
new n = unsafePrimToPrim $ igraphInit >>= igraphNew n directed >>= return . MGraph
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False

-- | Add nodes to the graph.
addNodes :: PrimMonad m
         => Int  -- ^ The number of new nodes.
         -> MGraph(PrimState m) d v e -> m ()
addNodes n (MGraph g) = unsafePrimToPrim $ igraphAddVertices g n nullPtr

-- | Add nodes with labels to the graph.
addLNodes :: (Serialize v, PrimMonad m)
          => [v]  -- ^ vertices' labels
          -> MGraph (PrimState m) d v e -> m ()
addLNodes labels (MGraph g) = unsafePrimToPrim $
    withAttr vertexAttr labels $ \attr ->
        withPtrs [attr] (igraphAddVertices g n . castPtr)
  where
    n = length labels

-- | Delete nodes from the graph.
delNodes :: PrimMonad m => [Int] -> MGraph (PrimState m) d v e -> m ()
delNodes ns (MGraph g) = unsafePrimToPrim $ withVerticesList ns $ \vs ->
    igraphDeleteVertices g vs

-- | Add edges to the graph.
addEdges :: PrimMonad m => [(Int, Int)] -> MGraph (PrimState m) d v e -> m ()
addEdges es (MGraph g) = unsafePrimToPrim $ withList xs $ \vec ->
    igraphAddEdges g vec nullPtr
  where
    xs = concatMap ( \(a,b) -> [a, b] ) es

-- | Add edges with labels to the graph.
addLEdges :: (PrimMonad m, Serialize e)
          => [LEdge e] -> MGraph (PrimState m) d v e -> m ()
addLEdges es (MGraph g) = unsafePrimToPrim $
    withAttr edgeAttr vs $ \attr -> withList (concat xs) $ \vec ->
        withPtrs [attr] (igraphAddEdges g vec . castPtr)
  where
    (xs, vs) = unzip $ map ( \((a,b),v) -> ([a, b], v) ) es

-- | Delete edges from the graph.
delEdges :: forall m d v e. (SingI d, PrimMonad m)
         => [(Int, Int)] -> MGraph (PrimState m) d v e -> m ()
delEdges es (MGraph g) = unsafePrimToPrim $ do
    eids <- forM es $ \(fr, to) -> igraphGetEid g fr to directed True
    withEdgeIdsList eids (igraphDeleteEdges g)
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False

-- | Set node attribute.
setNodeAttr :: (PrimMonad m, Serialize v)
            => Int   -- ^ Node id
            -> v
            -> MGraph (PrimState m) d v e
            -> m ()
setNodeAttr nodeId x (MGraph gr) = unsafePrimToPrim $
    withByteString (encode x) $ \bs -> do
        err <- igraphHaskellAttributeVASSet gr vertexAttr nodeId bs
        when (err /= 0) $ error "Fail to set node attribute!"

-- | Set edge attribute.
setEdgeAttr :: (PrimMonad m, Serialize e)
            => Int   -- ^ Edge id
            -> e
            -> MGraph (PrimState m) d v e
            -> m ()
setEdgeAttr edgeId x (MGraph gr) = unsafePrimToPrim $
    withByteString (encode x) $ \bs -> do
        err <- igraphHaskellAttributeEASSet gr edgeAttr edgeId bs
        when (err /= 0) $ error "Fail to set edge attribute!"
