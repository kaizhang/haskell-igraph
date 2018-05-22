{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Algorithms.Structure
    ( -- * Shortest Path Related Functions
      getShortestPath
    , inducedSubgraph
    , decompose
    , closeness
    , betweenness
    , eigenvectorCentrality
    , pagerank
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
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}

#include "haskell_igraph.h"

{#fun igraph_shortest_paths as ^
    { `IGraph'
    , castPtr `Ptr Matrix'
    , castPtr %`Ptr VertexSelector'
    , castPtr %`Ptr VertexSelector'
    , `Neimode'
    } -> `CInt' void- #}

-- Calculates and returns a single unweighted shortest path from a given vertex
-- to another one. If there are more than one shortest paths between the two
-- vertices, then an arbitrary one is returned.
getShortestPath :: Graph d v e
                -> Node     -- ^ The id of the source vertex.
                -> Node     -- ^ The id of the target vertex.
                -> [Node]
getShortestPath gr s t = unsafePerformIO $ allocaVector $ \path -> do
    igraphGetShortestPath (_graph gr) path nullPtr s t IgraphOut
    map truncate <$> toList path
{#fun igraph_get_shortest_path as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , `Int'
    , `Int'
    , `Neimode'
    } -> `CInt' void- #}

inducedSubgraph :: (Ord v, Serialize v)
                => Graph d v e
                -> [Int]
                -> Graph d v e
inducedSubgraph gr nds = unsafePerformIO $ withVerticesList nds $ \vs ->
    igraphInducedSubgraph (_graph gr) vs IgraphSubgraphCreateFromScratch >>=
        (\g -> return $ Graph g $ mkLabelToId g)
{#fun igraph_induced_subgraph as ^
    { `IGraph'
    , allocaIGraph- `IGraph' addIGraphFinalizer*
    , castPtr %`Ptr VertexSelector'
    , `SubgraphImplementation'
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

-- | Closeness centrality
closeness :: [Int]  -- ^ vertices
          -> Graph d v e
          -> Maybe [Double]  -- ^ optional edge weights
          -> Neimode
          -> Bool   -- ^ whether to normalize
          -> [Double]
closeness nds gr ws mode normal = unsafePerformIO $ allocaVector $ \result ->
    withVerticesList nds $ \vs -> withListMaybe ws $ \ws' -> do
        igraphCloseness (_graph gr) result vs mode ws' normal
        toList result
{#fun igraph_closeness as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr %`Ptr VertexSelector'
    , `Neimode'
    , castPtr `Ptr Vector'
    , `Bool' } -> `CInt' void- #}


-- | Betweenness centrality
betweenness :: [Int]
            -> Graph d v e
            -> Maybe [Double]
            -> [Double]
betweenness nds gr ws = unsafePerformIO $ allocaVector $ \result ->
    withVerticesList nds $ \vs -> withListMaybe ws $ \ws' -> do
        igraphBetweenness (_graph gr) result vs True ws' False
        toList result
{#fun igraph_betweenness as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr %`Ptr VertexSelector'
    , `Bool'
    , castPtr `Ptr Vector'
    , `Bool' } -> `CInt' void- #}

-- | Eigenvector centrality
eigenvectorCentrality :: Graph d v e
                      -> Maybe [Double]
                      -> [Double]
eigenvectorCentrality gr ws = unsafePerformIO $ allocaArpackOpt $ \arparck ->
    allocaVector $ \result -> withListMaybe ws $ \ws' -> do
        igraphEigenvectorCentrality (_graph gr) result nullPtr True True ws' arparck
        toList result
{#fun igraph_eigenvector_centrality as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , id `Ptr CDouble'
    , `Bool'
    , `Bool'
    , castPtr `Ptr Vector'
    , castPtr `Ptr ArpackOpt' } -> `CInt' void- #}

-- | Google's PageRank algorithm, with option to
pagerank :: SingI d
         => Graph d v e
         -> Maybe [Double]  -- ^ Node weights or reset probability. If provided,
                            -- the personalized PageRank will be used
         -> Maybe [Double]  -- ^ Edge weights
         -> Double  -- ^ damping factor, usually around 0.85
         -> [Double]
pagerank gr reset ws d
    | n == 0 = []
    | isJust ws && length (fromJust ws) /= m = error "incorrect length of edge weight vector"
    | isJust reset && length (fromJust reset) /= n = error
        "incorrect length of node weight vector"
    | fmap (foldl' (+) 0) reset == Just 0 = error "sum of node weight vector must be non-zero"
    | otherwise = unsafePerformIO $ alloca $ \p -> allocaVector $ \result ->
        withVerticesAll $ \vs -> withListMaybe ws $ \ws' -> do
            case reset of
                Nothing -> igraphPagerank (_graph gr) IgraphPagerankAlgoPrpack
                    result p vs (isDirected gr) d ws' nullPtr
                Just reset' -> withList reset' $ \reset'' -> igraphPersonalizedPagerank
                    (_graph gr) IgraphPagerankAlgoPrpack result p vs
                    (isDirected gr) d reset'' ws' nullPtr
            toList result
  where
    n = nNodes gr
    m = nEdges gr

{#fun igraph_pagerank as ^
    { `IGraph'
    , `PagerankAlgo'
    , castPtr `Ptr Vector'
    , id `Ptr CDouble'
    , castPtr %`Ptr VertexSelector'
    , `Bool'
    , `Double'
    , castPtr `Ptr Vector'
    , id `Ptr ()'
    } -> `CInt' void- #}

{#fun igraph_personalized_pagerank as ^
    { `IGraph'
    , `PagerankAlgo'
    , castPtr `Ptr Vector'
    , id `Ptr CDouble'
    , castPtr %`Ptr VertexSelector'
    , `Bool'
    , `Double'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , id `Ptr ()'
    } -> `CInt' void- #}
