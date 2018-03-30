module IGraph.Clique
    ( cliques
    , maximalCliques
    ) where

import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)

import IGraph
import IGraph.Internal.Clique
import IGraph.Internal.Data

cliques :: LGraph d v e
        -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                       -- No bound will be used if negative or zero
        -> [[Int]]     -- ^ cliques represented by node ids
cliques gr (lo, hi) = unsafePerformIO $ do
    vpptr <- igraphVectorPtrNew 0
    _ <- igraphCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr

maximalCliques :: LGraph d v e
               -> (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                              -- No bound will be used if negative or zero
               -> [[Int]]     -- ^ cliques represented by node ids
maximalCliques gr (lo, hi) = unsafePerformIO $ do
    vpptr <- igraphVectorPtrNew 0
    _ <- igraphMaximalCliques (_graph gr) vpptr lo hi
    (map.map) truncate <$> toLists vpptr
