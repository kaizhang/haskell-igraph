{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph.Mutable where

import           Control.Monad                  (when)
import           Control.Monad.Primitive
import qualified Data.ByteString.Char8          as B
import           Data.Serialize                 (Serialize)
import           Foreign

import           IGraph.Internal.Attribute
import           IGraph.Internal.Data
import           IGraph.Internal.Graph
import           IGraph.Internal.Initialization
import           IGraph.Internal.Selector

-- constants
vertexAttr :: String
vertexAttr = "vertex_attribute"

edgeAttr :: String
edgeAttr = "edge_attribute"

type LEdge a = (Int, Int, a)

-- | Mutable labeled graph
newtype MLGraph m d v e = MLGraph IGraphPtr

class MGraph d where
    new :: PrimMonad m => Int -> m (MLGraph (PrimState m) d v e)

    addNodes :: PrimMonad m => Int -> MLGraph(PrimState m) d v e -> m ()
    addNodes n (MLGraph g) = unsafePrimToPrim $ igraphAddVertices g n nullPtr

    addLNodes :: (Serialize v, PrimMonad m)
              => Int  -- ^ the number of new vertices add to the graph
              -> [v]  -- ^ vertices' labels
              -> MLGraph (PrimState m) d v e -> m ()
    addLNodes n labels (MLGraph g)
        | n /= length labels = error "addLVertices: incorrect number of labels"
        | otherwise = unsafePrimToPrim $ do
            with (makeAttributeRecord vertexAttr labels) $ \ptr -> do
                vptr <- listToVectorP [castPtr ptr]
                withVectorPPtr vptr $ \p -> igraphAddVertices g n $ castPtr p

    delNodes :: PrimMonad m => [Int] -> MLGraph (PrimState m) d v e -> m ()
    delNodes ns (MLGraph g) = unsafePrimToPrim $ do
        vptr <- listToVector $ map fromIntegral ns
        vsptr <- igraphVsVector vptr
        igraphDeleteVertices g vsptr
        return ()

    addEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()

    addLEdges :: (PrimMonad m, Serialize e) => [LEdge e] -> MLGraph (PrimState m) d v e -> m ()

    delEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()

data U = U
data D = D

instance MGraph U where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n False >>= return . MLGraph

    addEdges es (MLGraph g) = unsafePrimToPrim $ do
        vec <- listToVector xs
        igraphAddEdges g vec nullPtr
      where
        xs = concatMap ( \(a,b) -> [fromIntegral a, fromIntegral b] ) es

    addLEdges es (MLGraph g) = unsafePrimToPrim $ do
        vec <- listToVector $ concat xs
        let attr = makeAttributeRecord edgeAttr vs
        alloca $ \ptr -> do
            poke ptr attr
            vptr <- listToVectorP [castPtr ptr]
            withVectorPPtr vptr $ \p -> igraphAddEdges g vec $ castPtr p
      where
        (xs, vs) = unzip $ map ( \(a,b,v) -> ([fromIntegral a, fromIntegral b], v) ) es

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        vptr <- listToVector $ map fromIntegral eids
        esptr <- igraphEsVector vptr
        igraphDeleteEdges g esptr
        return ()
      where
        eids = flip map es $ \(fr, to) -> igraphGetEid g fr to False True

instance MGraph D where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n True >>= return . MLGraph

    addEdges es (MLGraph g) = unsafePrimToPrim $ do
        vec <- listToVector xs
        igraphAddEdges g vec nullPtr
      where
        xs = concatMap ( \(a,b) -> [fromIntegral a, fromIntegral b] ) es

    addLEdges es (MLGraph g) = unsafePrimToPrim $ do
        vec <- listToVector $ concat xs
        let attr = makeAttributeRecord edgeAttr vs
        alloca $ \ptr -> do
            poke ptr attr
            vptr <- listToVectorP [castPtr ptr]
            withVectorPPtr vptr $ \p -> igraphAddEdges g vec $ castPtr p
      where
        (xs, vs) = unzip $ map ( \(a,b,v) -> ([fromIntegral a, fromIntegral b], v) ) es

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        vptr <- listToVector $ map fromIntegral eids
        esptr <- igraphEsVector vptr
        igraphDeleteEdges g esptr
        return ()
      where
        eids = flip map es $ \(fr, to) -> igraphGetEid g fr to True True

setNodeAttr :: (PrimMonad m, Serialize v)
            => Int   -- ^ Node id
            -> v
            -> MLGraph (PrimState m) d v e
            -> m ()
setNodeAttr nodeId x (MLGraph gr) = unsafePrimToPrim $ do
    v <- unsafeToBS x
    with v $ \vptr -> do
        err <- igraphHaskellAttributeVASSet gr vertexAttr nodeId vptr
        when (err /= 0) $ error "Fail to set node attribute!"

setEdgeAttr :: (PrimMonad m, Serialize v)
            => Int   -- ^ Edge id
            -> v
            -> MLGraph (PrimState m) d v e
            -> m ()
setEdgeAttr edgeId x (MLGraph gr) = unsafePrimToPrim $ do
    v <- unsafeToBS x
    with v $ \vptr -> do
        err <- igraphHaskellAttributeEASSet gr edgeAttr edgeId vptr
        when (err /= 0) $ error "Fail to set edge attribute!"
