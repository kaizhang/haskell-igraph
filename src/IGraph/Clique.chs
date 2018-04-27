{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Clique
    ( cliques
    , maximalCliques
    ) where

import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Foreign.Ptr as C2HSImp
import Foreign

import IGraph
{#import IGraph.Internal #}

#include "haskell_igraph.h"

cliques :: LGraph d v e
        -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                       -- No bound will be used if negative or zero
        -> [[Int]]     -- ^ cliques represented by node ids
cliques gr (lo, hi) = unsafePerformIO $ allocaVectorPtr $ \vpptr -> do
    igraphCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr
{#fun igraph_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr', `Int', `Int' } -> `CInt' void- #}

maximalCliques :: LGraph d v e
               -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                              -- No bound will be used if negative or zero
               -> [[Int]]     -- ^ cliques represented by node ids
maximalCliques gr (lo, hi) = unsafePerformIO $ allocaVectorPtr $ \vpptr -> do
    igraphMaximalCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr
{#fun igraph_maximal_cliques as ^ { `IGraph', castPtr `Ptr VectorPtr', `Int', `Int' } -> `CInt' void- #}
