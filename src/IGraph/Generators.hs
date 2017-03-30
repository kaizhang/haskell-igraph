module IGraph.Generators
    ( ErdosRenyiModel(..)
    , erdosRenyiGame
    ) where

import IGraph
import IGraph.Mutable
import IGraph.Internal.Graph
import IGraph.Internal.Constants
import IGraph.Internal.Initialization

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
