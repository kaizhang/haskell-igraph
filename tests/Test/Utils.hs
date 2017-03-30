module Test.Utils where

import Control.Monad
import System.Random
import Data.List.Ordered

randEdges :: Int  -- ^ number of edges to generate
          -> Int  -- ^ number of nodes in the graph
          -> IO [(Int, Int)]
randEdges n nd = do
    fr <- replicateM (2*n) $ randomRIO (0,nd-1)
    to <- replicateM (2*n) $ randomRIO (0,nd-1)
    return $ take n $ nubSort $ filter (uncurry (/=)) $ zip fr to
