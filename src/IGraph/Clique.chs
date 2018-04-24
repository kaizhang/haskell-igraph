{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Clique
    ( cliques
    , maximalCliques
    ) where

import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Foreign.Marshal.Utils as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import Foreign
import Foreign.C.Types

import IGraph
{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "haskell_igraph.h"

cliques :: LGraph d v e
        -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                       -- No bound will be used if negative or zero
        -> [[Int]]     -- ^ cliques represented by node ids
cliques gr (lo, hi) = unsafePerformIO $ do
    vpptr <- igraphVectorPtrNew 0
    _ <- igraphCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr
{#fun igraph_cliques as ^ { `IGraph', `VectorPtr', `Int', `Int' } -> `Int' #}

maximalCliques :: LGraph d v e
               -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                              -- No bound will be used if negative or zero
               -> [[Int]]     -- ^ cliques represented by node ids
maximalCliques gr (lo, hi) = unsafePerformIO $ do
    vpptr <- igraphVectorPtrNew 0
    _ <- igraphMaximalCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr
{#fun igraph_maximal_cliques as ^ { `IGraph', `VectorPtr', `Int', `Int' } -> `Int' #}
