module IGraph.Clique
    ( cliques
    , maximalCliques
    ) where

import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)

import IGraph
import IGraph.Internal.Clique
import IGraph.Internal.Data

cliques :: (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                       -- No bound will be used if negative or zero
        -> LGraph d v e
        -> [[Int]]     -- ^ cliques represented by node ids
cliques (lo, hi) (LGraph g) = unsafePerformIO $ allocaVectorP $ \vpptr -> do
    _ <- igraphCliques g vpptr lo hi
    (map.map) truncate <$> vectorPPtrToList vpptr

maximalCliques :: (Int, Int)  -- ^ Minimum and maximum size of the cliques to be returned.
                              -- No bound will be used if negative or zero
               -> LGraph d v e
               -> [[Int]]     -- ^ cliques represented by node ids
maximalCliques (lo, hi) (LGraph g) = unsafePerformIO $ allocaVectorP $ \vpptr -> do
    _ <- igraphMaximalCliques g vpptr lo hi
    (map.map) truncate <$> vectorPPtrToList vpptr
