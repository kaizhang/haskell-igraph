{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
module IGraph.Algorithms.Structure
    ( -- * Shortest Path Related Functions
      shortestPath
    , averagePathLength
    , diameter
    , eccentricity
    , radius
      -- * Graph Components
    , inducedSubgraph
    , isConnected
    , isStronglyConnected
    , decompose
    , articulationPoints
    , bridges
      -- * Topological Sorting, Directed Acyclic Graphs
    , isDag
    , topSort
    , topSortUnsafe
      -- * Other Operations
    , density
    , reciprocity
      -- * Auxiliary types
    , Neimode(IgraphOut,IgraphIn,IgraphAll) -- not IgraphTotal
    ) where

import           Control.Monad
import           Data.Serialize            (Serialize)
import           System.IO.Unsafe          (unsafePerformIO)
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

-- | Calculates the average shortest path length between all vertex pairs.
averagePathLength :: SingI d
                  => Graph d v e
                  -> Bool     -- ^ if unconnected,
                              -- include only connected pairs (True)
                              -- or return number if vertices (False)
                  -> Double
averagePathLength graph unconn =
  cFloatConv $ igraphAveragePathLength (_graph graph) (isDirected graph) unconn
{-# INLINE igraphAveragePathLength #-}
{#fun pure igraph_average_path_length as ^
    { `IGraph'
    , alloca- `CDouble' peek*
    , `Bool'
    , `Bool'
    } -> `CInt' void- #}

-- | Calculates the diameter of a graph (longest geodesic).
diameter :: SingI d
         => Graph d v e
         -> Bool     -- ^ if unconnected,
                     -- return largest component diameter (True)
                     -- or number of vertices (False)
         -> (Int, [Node])
diameter graph unconn = unsafePerformIO $
  alloca $ \pres ->
  allocaVector $ \path -> do
    igraphDiameter (_graph graph) pres nullPtr nullPtr path (isDirected graph) unconn
    liftM2 (,) (peekIntConv pres) (toNodes path)
{-# INLINE igraphDiameter #-}
{#fun igraph_diameter as ^
    { `IGraph'
    , castPtr `Ptr CInt'
    , castPtr `Ptr CInt'
    , castPtr `Ptr CInt'
    , castPtr `Ptr Vector'
    , `Bool'
    , `Bool'
    } -> `CInt' void- #}

-- | Eccentricity of some vertices.
eccentricity :: Graph d v e
             -> Neimode -- ^ 'IgraphOut' to follow edges' direction,
                        -- 'IgraphIn' to reverse it, 'IgraphAll' to ignore
             -> [Node]  -- ^ vertices for which to calculate eccentricity
             -> [Double]
eccentricity graph mode vids = unsafePerformIO $
  allocaVector $ \res ->
  withVerticesList vids $ \vs -> do
    igraphEccentricity (_graph graph) res vs mode
    toList res
{-# INLINE igraphEccentricity #-}
{#fun igraph_eccentricity as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr %`Ptr VertexSelector'
    , `Neimode'
    } -> `CInt' void- #}

-- | Radius of a graph.
radius :: Graph d v e
       -> Neimode -- ^ 'IgraphOut' to follow edges' direction,
                  -- 'IgraphIn' to reverse it, 'IgraphAll' to ignore
       -> Double
radius graph mode = cFloatConv $ igraphRadius (_graph graph) mode
{-# INLINE igraphRadius #-}
{#fun pure igraph_radius as ^
    { `IGraph'
    , alloca- `CDouble' peek*
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

-- | Find the articulation points in a graph.
articulationPoints :: Graph d v e -> [Node]
articulationPoints gr = unsafePerformIO $ allocaVector $ \res -> do
  igraphArticulationPoints (_graph gr) res
  toNodes res
{-#INLINE igraphArticulationPoints #-}
{#fun igraph_articulation_points as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    } -> `CInt' void- #}

-- ^ Find all bridges in a graph.
bridges :: Graph d v e -> [Edge]
bridges gr = unsafePerformIO $ allocaVector $ \res -> do
  igraphBridges (_graph gr) res
  map (getEdgeByEid gr) <$> toNodes res
{-# INLINE igraphBridges #-}
{#fun igraph_bridges as ^
    { `IGraph'
    , castPtr `Ptr Vector'
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

-- | Calculate the density of a graph.
density :: Graph d v e
        -> Bool -- ^ whether to include loops
        -> Double -- ^ the ratio of edges to possible edges
density gr loops = unsafePerformIO $ alloca $ \res -> do
  igraphDensity (_graph gr) res loops
  peek res
{-# INLINE igraphDensity #-}
{#fun igraph_density as ^
    { `IGraph'
    , castPtr `Ptr Double'
    , `Bool'
    } -> `CInt' void -#}

-- | Calculates the reciprocity of a directed graph.
reciprocity :: Graph d v e
            -> Bool -- ^ whether to ignore loop edges
            -> Double -- ^ the proportion of mutual connections
reciprocity gr ignore_loops = unsafePerformIO $ alloca $ \res -> do
  igraphReciprocity (_graph gr) res ignore_loops IgraphReciprocityDefault
  peek res
{#fun igraph_reciprocity as ^
    { `IGraph'
    , castPtr `Ptr Double'
    , `Bool'
    , `Reciprocity'
    } -> `CInt' void -#}