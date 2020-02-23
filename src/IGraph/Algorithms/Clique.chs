{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Algorithms.Clique
    ( cliques
    , largestCliques
    , maximalCliques
    , cliqueNumber
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Foreign

import IGraph
import IGraph.Internal.C2HS
{#import IGraph.Internal #}

#include "haskell_igraph.h"

-- | Find all or some cliques in a graph.
cliques :: Graph d v e
        -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                       -- No bound will be used if negative or zero
        -> [[Int]]     -- ^ cliques represented by node ids
cliques gr (lo, hi) = unsafePerformIO $ allocaVectorPtr $ \vptr -> do
    igraphCliques (_graph gr) vptr lo hi
    (map.map) truncate <$> toLists vptr
{#fun igraph_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr', `Int', `Int' } -> `CInt' void- #}

-- | Finds the largest clique(s) in a graph.
-- Time complexity: O(3^(|V|/3)) worst case.
largestCliques :: Graph d v e -> [[Int]]
largestCliques gr = unsafePerformIO $ allocaVectorPtr $ \vptr -> do
    igraphLargestCliques (_graph gr) vptr
    (map.map) truncate <$> toLists vptr
{#fun igraph_largest_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr' } -> `CInt' void- #}

-- | Find all maximal cliques of a graph. Time complexity: O(d(n-d)3^(d/3))
-- worst case, d is the degeneracy of the graph.
maximalCliques :: Graph d v e
               -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                              -- No bound will be used if negative or zero
               -> [[Int]]     -- ^ cliques represented by node ids
maximalCliques gr (lo, hi) = unsafePerformIO $ allocaVectorPtr $ \vpptr -> do
    igraphMaximalCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr
{#fun igraph_maximal_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr', `Int', `Int' } -> `CInt' void- #}

-- | Find the clique number of the graph. The clique number of a graph is
-- the size of the largest clique.
-- Time complexity: O(3^(|V|/3)) worst case.
cliqueNumber :: Graph d v e -> Int
cliqueNumber gr = unsafePerformIO $ igraphCliqueNumber $ _graph gr
{#fun igraph_clique_number as ^
    { `IGraph'
    , alloca- `Int' peekIntConv*
    } -> `CInt' void- #}