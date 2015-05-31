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

class MGraph gr d where
    new :: PrimMonad m => Int -> m (gr (PrimState m) d v e)

    addVertices :: PrimMonad m => Int -> gr (PrimState m) d v e -> m ()

    addLVertices :: (Show v, PrimMonad m)
                 => Int  -- ^ the number of new vertices add to the graph
                 -> [v]  -- ^ vertices' labels
                 -> gr (PrimState m) d v e -> m ()

    addEdges :: PrimMonad m => [(Int, Int)] -> gr (PrimState m) d v e -> m ()

    addLEdges :: (PrimMonad m, Show e) => [LEdge e] -> gr (PrimState m) d v e -> m ()

-- | Mutable labeled graph
newtype MLGraph m d v e = MLGraph IGraphPtr

data U
data D

instance MGraph MLGraph U where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n False >>= return . MLGraph

    addVertices n (MLGraph g) = unsafePrimToPrim $ igraphAddVertices g n nullPtr

    addLVertices n labels (MLGraph g)
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
