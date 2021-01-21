{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IGraph.Algorithms.Generators
    ( full
    , star
    , ring
    , zacharyKarate
    , ErdosRenyiModel(..)
    , erdosRenyiGame
    , degreeSequenceGame
    , rewireEdges
    , rewire
    ) where

import           Data.Serialize                 (Serialize)
import Data.Singletons (SingI, Sing, sing, fromSing)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as M
import Control.Monad.Primitive (RealWorld)

import qualified Foreign.Ptr as C2HSImp
import Foreign

import           IGraph
import           IGraph.Random (Gen)
import           IGraph.Mutable (MGraph(..))
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}
{# import IGraph.Internal.Initialization #}

#include "haskell_igraph.h"

full :: forall d. SingI d
     => Int   -- ^ The number of vertices in the graph.
     -> Bool  -- ^ Whether to include self-edges (loops)
     -> Graph d () ()
full n hasLoop = unsafePerformIO $ do
    _ <- igraphInit
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
    _ <- igraphInit
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
    _ <- igraphInit
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

-- | Zachary's karate club
zacharyKarate :: Graph 'U () ()
zacharyKarate = mkGraph (replicate 34 ()) $ map (\(a, b) -> ((a-1,b-1),())) es
  where
    es = [ (2,1),(3,1),(3,2),(4,1),(4,2),(4,3),(5,1),(6,1),(7,1),(7,5),(7,6)
         , (8,1),(8,2),(8,3),(8,4),(9,1),(9,3),(10,3),(11,1),(11,5),(11,6)
         , (12,1),(13,1),(13,4),(14,1),(14,2),(14,3),(14,4),(17,6),(17,7)
         , (18,1),(18,2),(20,1),(20,2),(22,1),(22,2),(26,24),(26,25)
         , (28,3),(28,24),(28,25),(29,3),(30,24),(30,27),(31,2),(31,9)
         , (32,1),(32,25),(32,26),(32,29),(33,3),(33,9),(33,15),(33,16)
         , (33,19),(33,21),(33,23),(33,24),(33,30),(33,31),(33,32)
         , (34,9),(34,10),(34,14),(34,15),(34,16),(34,19),(34,20),(34,21)
         , (34,23),(34,24),(34,27),(34,28),(34,29),(34,30),(34,31),(34,32),(34,33) ]

data ErdosRenyiModel = GNP Int Double  -- ^ G(n,p) graph, every possible edge is
                                       -- included in the graph with probability p.
                     | GNM Int Int   -- ^ G(n,m) graph, m edges are selected
                                     -- uniformly randomly in a graph with n
                                     -- vertices.

erdosRenyiGame :: forall d. SingI d
               => ErdosRenyiModel
               -> Bool  -- ^ self-loop
               -> Gen
               -> IO (Graph d () ())
erdosRenyiGame model self _ = do
    _ <- igraphInit
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
                   -> Gen
                   -> IO (Graph 'D () ())
degreeSequenceGame out_deg in_deg _ = do
    _ <- igraphInit
    withList out_deg $ \out_deg' ->
        withList in_deg $ \in_deg' -> do
            gr <- igraphDegreeSequenceGame out_deg' in_deg' IgraphDegseqSimple
            initializeNullAttribute gr
            return $ Graph gr M.empty
{#fun igraph_degree_sequence_game as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , castPtr `Ptr Vector', castPtr `Ptr Vector', `Degseq'
    } -> `CInt' void- #}


-- | Rewire the edges of a graph with constant probability.
rewireEdges :: MGraph RealWorld d v e
            -> Double   -- ^ The rewiring probability a constant between zero and
                        -- one (inclusive).
            -> Bool     -- ^ whether loop edges are allowed in the new graph, or not.
            -> Bool     -- ^ whether multiple edges are allowed in the new graph.
            -> Gen
            -> IO ()
rewireEdges gr p loop multi _ = igraphRewireEdges (_mgraph gr) p loop multi
{#fun igraph_rewire_edges as ^ 
    { `IGraph'
    , `Double'
    , `Bool'
    , `Bool'
    } -> `CInt' void- #}

-- | Randomly rewires a graph while preserving the degree distribution.
rewire :: (Serialize v, Ord v, Serialize e)
       => Int    -- ^ Number of rewiring trials to perform.
       -> Graph d v e
       -> Gen
       -> IO (Graph d v e)
rewire n gr _ = do
    gr' <- thaw gr
    igraphRewire (_mgraph gr') n IgraphRewiringSimple
    unsafeFreeze gr'
{#fun igraph_rewire as ^ { `IGraph', `Int', `Rewiring' } -> `CInt' void-#}

