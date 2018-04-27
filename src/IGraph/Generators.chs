{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Generators
    ( full
    , ErdosRenyiModel(..)
    , erdosRenyiGame
    , degreeSequenceGame
    , rewire
    ) where

import           Control.Monad                  (when)
import           Data.Hashable                  (Hashable)
import           Data.Serialize                 (Serialize)

import qualified Foreign.Ptr as C2HSImp
import Foreign

import           IGraph
import           IGraph.Mutable
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}
{# import IGraph.Internal.Initialization #}

#include "haskell_igraph.h"

{#fun igraph_full as full
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `Int', `Bool', `Bool'
    } -> `CInt' void- #}

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
{#fun igraph_erdos_renyi_game as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `ErdosRenyi', `Int', `Double', `Bool', `Bool'
    } -> `CInt' void- #}

-- | Generates a random graph with a given degree sequence.
degreeSequenceGame :: [Int]   -- ^ Out degree
                   -> [Int]   -- ^ In degree
                   -> IO (LGraph D () ())
degreeSequenceGame out_deg in_deg = withList out_deg $ \out_deg' ->
    withList in_deg $ \in_deg' -> do
        gp <- igraphDegreeSequenceGame out_deg' in_deg' IgraphDegseqSimple
        unsafeFreeze $ MLGraph gp
{#fun igraph_degree_sequence_game as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , castPtr `Ptr Vector', castPtr `Ptr Vector', `Degseq'
    } -> `CInt' void- #}

-- | Randomly rewires a graph while preserving the degree distribution.
rewire :: (Graph d, Hashable v, Serialize v, Eq v, Serialize e)
       => Int    -- ^ Number of rewiring trials to perform.
       -> LGraph d v e
       -> IO (LGraph d v e)
rewire n gr = do
    (MLGraph gptr) <- thaw gr
    err <- igraphRewire gptr n IgraphRewiringSimple
    when (err /= 0) $ error "failed to rewire graph!"
    unsafeFreeze $ MLGraph gptr
{#fun igraph_rewire as ^ { `IGraph', `Int', `Rewiring' } -> `Int' #}
