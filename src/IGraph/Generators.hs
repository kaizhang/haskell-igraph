module IGraph.Generators
    ( ErdosRenyiModel(..)
    , erdosRenyiGame
    ) where

import IGraph
import IGraph.Mutable
import IGraph.Internal.Graph
import IGraph.Internal.Constants
import IGraph.Internal.Initialization

data ErdosRenyiModel = GNP
                     | GNM

erdosRenyiGame :: Graph d
               => ErdosRenyiModel
               -> Int   -- ^ n
               -> Double   -- ^ p or m
               -> d     -- ^ directed
               -> Bool  -- ^ self-loop
               -> IO (LGraph d () ())
erdosRenyiGame model n p_or_m d self = do
    gp <- igraphInit >> igraphErdosRenyiGame model' n p_or_m (isD d) self
    unsafeFreeze $ MLGraph gp
  where
    model' = case model of
        GNP -> IgraphErdosRenyiGnp
        GNM -> IgraphErdosRenyiGnm
