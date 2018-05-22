{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IGraph.Algorithms.Generators
    ( full
    , star
    , ring
    , ErdosRenyiModel(..)
    , erdosRenyiGame
    , degreeSequenceGame
    , rewire
    ) where

import           Control.Monad                  (when, forM_)
import           Data.Serialize                 (Serialize)
import Data.Singletons (SingI, Sing, sing, fromSing)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as M

import qualified Foreign.Ptr as C2HSImp
import Foreign

import           IGraph
import           IGraph.Mutable (MGraph(..))
import qualified IGraph.Mutable as GM
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}
{# import IGraph.Internal.Initialization #}

#include "haskell_igraph.h"

full :: forall d. SingI d
     => Int   -- ^ The number of vertices in the graph.
     -> Bool  -- ^ Whether to include self-edges (loops)
     -> Graph d () ()
full n hasLoop = unsafePerformIO $ do
    igraphInit
    gr <- igraphFull n directed hasLoop
    initializeNullAttribute gr
    return $ Graph gr M.empty
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False
{#fun igraph_full as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `Int', `Bool', `Bool'
    } -> `CInt' void- #}

-- | Return the Star graph. The center node is always associated with id 0.
star :: Int    -- ^ The number of nodes
     -> Graph 'U () ()
star n = unsafePerformIO $ do
    igraphInit
    gr <- igraphStar n IgraphStarUndirected 0
    initializeNullAttribute gr
    return $ Graph gr M.empty
{#fun igraph_star as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `Int'
    , `StarMode'
    , `Int'
    } -> `CInt' void- #}

-- | Creates a ring graph, a one dimensional lattice.
ring :: Int -> Graph 'U () ()
ring n = unsafePerformIO $ do
    igraphInit
    gr <- igraphRing n False False True
    initializeNullAttribute gr
    return $ Graph gr M.empty
{#fun igraph_ring as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `Int'
    , `Bool'
    , `Bool'
    , `Bool'
    } -> `CInt' void- #}

data ErdosRenyiModel = GNP Int Double
                     | GNM Int Int

erdosRenyiGame :: forall d. SingI d
               => ErdosRenyiModel
               -> Bool  -- ^ self-loop
               -> IO (Graph d () ())
erdosRenyiGame model self = do
    igraphInit
    gr <- case model of
        GNP n p -> igraphErdosRenyiGame IgraphErdosRenyiGnp n p directed self
        GNM n m -> igraphErdosRenyiGame IgraphErdosRenyiGnm n (fromIntegral m)
            directed self
    initializeNullAttribute gr
    return $ Graph gr M.empty
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False
{#fun igraph_erdos_renyi_game as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `ErdosRenyi', `Int', `Double', `Bool', `Bool'
    } -> `CInt' void- #}

-- | Generates a random graph with a given degree sequence.
degreeSequenceGame :: [Int]   -- ^ Out degree
                   -> [Int]   -- ^ In degree
                   -> IO (Graph 'D () ())
degreeSequenceGame out_deg in_deg = do
    igraphInit
    withList out_deg $ \out_deg' ->
        withList in_deg $ \in_deg' -> do
            gr <- igraphDegreeSequenceGame out_deg' in_deg' IgraphDegseqSimple
            initializeNullAttribute gr
            return $ Graph gr M.empty
{#fun igraph_degree_sequence_game as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , castPtr `Ptr Vector', castPtr `Ptr Vector', `Degseq'
    } -> `CInt' void- #}

-- | Randomly rewires a graph while preserving the degree distribution.
rewire :: (Serialize v, Ord v, Serialize e)
       => Int    -- ^ Number of rewiring trials to perform.
       -> Graph d v e
       -> IO (Graph d v e)
rewire n gr = do
    gr' <- thaw gr
    igraphRewire (_mgraph gr') n IgraphRewiringSimple
    unsafeFreeze gr'
{#fun igraph_rewire as ^ { `IGraph', `Int', `Rewiring' } -> `CInt' void-#}
