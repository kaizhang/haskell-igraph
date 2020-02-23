{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
module IGraph.Algorithms.Structure
    ( -- * Shortest Path Related Functions
      shortestPath
    , inducedSubgraph
    , isConnected
    , isStronglyConnected
    , decompose
    , isDag
    , topSort
    , topSortUnsafe
    ) where

import           Control.Monad
import           Data.Serialize            (Serialize)
import Data.List (foldl')
import           System.IO.Unsafe          (unsafePerformIO)
import Data.Maybe
import Data.Singletons (SingI)

import Foreign
import Foreign.C.Types

import           IGraph
import IGraph.Internal.C2HS
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}

#include "haskell_igraph.h"

-- Calculates and returns a single unweighted shortest path from a given vertex
-- to another one. If there are more than one shortest paths between the two
-- vertices, then an arbitrary one is returned.
shortestPath :: Serialize e
             => Graph d v e
             -> Node     -- ^ The id of the source vertex.
             -> Node     -- ^ The id of the target vertex.
             -> Maybe (e -> Double)  -- ^ A function to retrieve edge weights. If provied,
                                     -- the Dijkstra's algorithm will be used.
             -> [Node]
shortestPath gr s t getEdgeW = unsafePerformIO $ allocaVector $ \path -> do
    case getEdgeW of
        Nothing -> igraphGetShortestPath (_graph gr) path nullPtr s t IgraphOut
        Just f -> withList (map (f . snd) $ labEdges gr) $ \ws ->
            igraphGetShortestPathDijkstra (_graph gr) path nullPtr s t ws IgraphOut
    map truncate <$> toList path
{-# INLINE shortestPath #-}
{#fun igraph_get_shortest_path as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , `Int'
    , `Int'
    , `Neimode'
    } -> `CInt' void- #}
{#fun igraph_get_shortest_path_dijkstra as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , `Int'
    , `Int'
    , castPtr `Ptr Vector'
    , `Neimode'
    } -> `CInt' void- #}

-- | Creates a subgraph induced by the specified vertices. This function collects
-- the specified vertices and all edges between them to a new graph.
inducedSubgraph :: (Ord v, Serialize v)
                => Graph d v e
                -> [Node]
                -> Graph d v e
inducedSubgraph gr nds = unsafePerformIO $ withVerticesList nds $ \vs ->
    igraphInducedSubgraph (_graph gr) vs IgraphSubgraphCreateFromScratch >>=
        (\g -> return $ Graph g $ mkLabelToId g)
{-# INLINE inducedSubgraph #-}
{#fun igraph_induced_subgraph as ^
    { `IGraph'
    , allocaIGraph- `IGraph' addIGraphFinalizer*
    , castPtr %`Ptr VertexSelector'
    , `SubgraphImplementation'
    } -> `CInt' void- #}

-- | Decides whether the graph is weakly connected.
isConnected :: Graph d v e -> Bool
isConnected gr = igraphIsConnected (_graph gr) IgraphWeak
{-# INLINE isConnected #-}

isStronglyConnected :: Graph 'D v e -> Bool
isStronglyConnected gr = igraphIsConnected (_graph gr) IgraphStrong
{-# INLINE isStronglyConnected #-}
{#fun pure igraph_is_connected as ^
    { `IGraph'
    , alloca- `Bool' peekBool*
    , `Connectedness'
    } -> `CInt' void- #}

-- | Decompose a graph into connected components.
decompose :: (Ord v, Serialize v)
          => Graph d v e -> [Graph d v e]
decompose gr = unsafePerformIO $ allocaVectorPtr $ \ptr -> do
    igraphDecompose (_graph gr) ptr IgraphWeak (-1) 1
    n <- igraphVectorPtrSize ptr
    forM [0..n-1] $ \i -> do
        p <- igraphVectorPtrE ptr i
        addIGraphFinalizer (castPtr p) >>= (\g -> return $ Graph g $ mkLabelToId g)
{-# INLINE decompose #-}
{#fun igraph_decompose as ^
    { `IGraph'
    , castPtr `Ptr VectorPtr'
    , `Connectedness'
    , `Int'
    , `Int'
    } -> `CInt' void- #}


-- | Checks whether a graph is a directed acyclic graph (DAG) or not.
isDag :: Graph d v e -> Bool
isDag = igraphIsDag . _graph
{#fun pure igraph_is_dag as ^
    { `IGraph'
    , alloca- `Bool' peekBool*
    } -> `CInt' void- #}
{-# INLINE isDag #-}

-- | Calculate a possible topological sorting of the graph. Raise error if the
-- graph is not acyclic.
topSort :: Graph d v e -> [Node]
topSort gr | isDag gr = topSortUnsafe gr
           | otherwise = error "the graph is not acyclic"
{-# INLINE topSort #-}

-- | Calculate a possible topological sorting of the graph. If the graph is not
-- acyclic (it has at least one cycle), a partial topological sort is returned.
topSortUnsafe :: Graph d v e -> [Node]
topSortUnsafe gr = unsafePerformIO $ allocaVectorN n $ \res -> do
    igraphTopologicalSorting (_graph gr) res IgraphOut
    map truncate <$> toList res
  where
    n = nNodes gr
{-# INLINE topSortUnsafe #-}
{#fun igraph_topological_sorting as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , `Neimode'
    } -> `CInt' void- #}
