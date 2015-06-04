{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph.Mutable where

import Foreign
import Control.Monad.Primitive

import IGraph.Internal.Graph
import IGraph.Internal.Data
import IGraph.Internal.Attribute
import IGraph.Internal.Initialization

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

    addLNodes :: (Show v, PrimMonad m)
                 => Int  -- ^ the number of new vertices add to the graph
                 -> [v]  -- ^ vertices' labels
                 -> MLGraph (PrimState m) d v e -> m ()

    addEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()

    addLEdges :: (PrimMonad m, Show e) => [LEdge e] -> MLGraph (PrimState m) d v e -> m ()

data U
data D

instance MGraph U where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n False >>= return . MLGraph

    addNodes n (MLGraph g) = unsafePrimToPrim $ igraphAddVertices g n nullPtr

    addLNodes n labels (MLGraph g)
        | n /= length labels = error "addLVertices: incorrect number of labels"
        | otherwise = unsafePrimToPrim $ do
            let attr = makeAttributeRecord vertexAttr labels
            alloca $ \ptr -> do
                poke ptr attr
                vptr <- listToVectorP [castPtr ptr]
                withVectorPPtr vptr $ \p -> igraphAddVertices g n $ castPtr p

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
