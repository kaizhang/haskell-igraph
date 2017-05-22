module IGraph.Generators
    ( ErdosRenyiModel(..)
    , erdosRenyiGame
    , degreeSequenceGame
    , rewire
    ) where

import           Control.Monad                  (when)
import           Data.Hashable                  (Hashable)

import           IGraph
import           IGraph.Internal.Constants
import           IGraph.Internal.Data
import           IGraph.Internal.Graph
import           IGraph.Internal.Initialization
import           IGraph.Mutable

data ErdosRenyiModel = GNP Int Double
                     | GNM Int Int

erdosRenyiGame :: Graph d
               => ErdosRenyiModel
               -> d     -- ^ directed
               -> Bool  -- ^ self-loop
               -> IO (LGraph d () ())
erdosRenyiGame (GNP n p) d self = do
    gp <- igraphInit >> igraphErdosRenyiGame IgraphErdosRenyiGnp n p (isD d) self
    unsafeFreeze $ MLGraph gp
erdosRenyiGame (GNM n m) d self = do
    gp <- igraphInit >> igraphErdosRenyiGame IgraphErdosRenyiGnm n
        (fromIntegral m) (isD d) self
    unsafeFreeze $ MLGraph gp

-- | Generates a random graph with a given degree sequence.
degreeSequenceGame :: [Int]   -- ^ Out degree
                   -> [Int]   -- ^ In degree
                   -> IO (LGraph D () ())
degreeSequenceGame out_deg in_deg = do
    out_deg' <- listToVector $ map fromIntegral out_deg
    in_deg' <- listToVector $ map fromIntegral in_deg
    gp <- igraphDegreeSequenceGame out_deg' in_deg' IgraphDegseqSimple
    unsafeFreeze $ MLGraph gp

-- | Randomly rewires a graph while preserving the degree distribution.
rewire :: (Graph d, Hashable v, Read v, Eq v, Show v, Show e)
       => Int    -- ^ Number of rewiring trials to perform.
       -> LGraph d v e
       -> IO (LGraph d v e)
rewire n gr = do
    (MLGraph gptr) <- thaw gr
    err <- igraphRewire gptr n IgraphRewiringSimple
    when (err /= 0) $ error "failed to rewire graph!"
    unsafeFreeze $ MLGraph gptr
