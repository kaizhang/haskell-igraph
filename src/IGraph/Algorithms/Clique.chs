{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Algorithms.Clique
    ( cliques
    , largestCliques
    , maximalCliques
    , cliqueNumber
    ) where

import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Foreign.Ptr as C2HSImp
import Foreign

import IGraph
import IGraph.Internal.C2HS
{#import IGraph.Internal #}

#include "haskell_igraph.h"

cliques :: Graph d v e
        -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                       -- No bound will be used if negative or zero
        -> [[Int]]     -- ^ cliques represented by node ids
cliques gr (lo, hi) = unsafePerformIO $ allocaVectorPtr $ \vptr -> do
    igraphCliques (_graph gr) vptr lo hi
    (map.map) truncate <$> toLists vptr
{#fun igraph_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr', `Int', `Int' } -> `CInt' void- #}

largestCliques :: Graph d v e -> [[Int]]
largestCliques gr = unsafePerformIO $ allocaVectorPtr $ \vptr -> do
    igraphLargestCliques (_graph gr) vptr
    (map.map) truncate <$> toLists vptr
{#fun igraph_largest_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr' } -> `CInt' void- #}

maximalCliques :: Graph d v e
               -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                              -- No bound will be used if negative or zero
               -> [[Int]]     -- ^ cliques represented by node ids
maximalCliques gr (lo, hi) = unsafePerformIO $ allocaVectorPtr $ \vpptr -> do
    igraphMaximalCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr
{#fun igraph_maximal_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr', `Int', `Int' } -> `CInt' void- #}

cliqueNumber :: Graph d v e -> Int
cliqueNumber gr = unsafePerformIO $ igraphCliqueNumber $ _graph gr
{#fun igraph_clique_number as ^
    { `IGraph'
    , alloca- `Int' peekIntConv*
    } -> `CInt' void- #}
