{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
module IGraph.Algorithms.Motif
    ( triad
    , triadCensus
    ) where

import Data.Hashable (Hashable)
import System.IO.Unsafe (unsafePerformIO)

import Foreign

import IGraph
{#import IGraph.Internal #}

#include "haskell_igraph.h"

-- | Every triple of vertices in a directed graph
-- 003: A, B, C, the empty graph.
-- 012: A->B, C, a graph with a single directed edge.
-- 102: A<->B, C, a graph with a mutual connection between two vertices.
-- 021D: A<-B->C, the binary out-tree.
-- 021U: A->B<-C, the binary in-tree.
-- 021C: A->B->C, the directed line.
-- 111D: A<->B<-C.
-- 111U: A<->B->C.
-- 030T: A->B<-C, A->C. Feed forward loop.
-- 030C: A<-B<-C, A->C.
-- 201: A<->B<->C.
-- 120D: A<-B->C, A<->C.
-- 120U: A->B<-C, A<->C.
-- 120C: A->B->C, A<->C.
-- 210: A->B<->C, A<->C.
-- 300: A<->B<->C, A<->C, the complete graph.
triad :: [Graph 'D () ()]
triad = map make edgeList
  where
    edgeList =
         [ []
         , [(0,1)]
         , [(0,1), (1,0)]
         , [(1,0), (1,2)]
         , [(0,1), (2,1)]
         , [(0,1), (1,2)]
         , [(0,1), (1,0), (2,1)]
         , [(0,1), (1,0), (1,2)]
         , [(0,1), (2,1), (0,2)]
         , [(1,0), (2,1), (0,2)]
         , [(0,1), (1,0), (0,2), (2,0)]
         , [(1,0), (1,2), (0,2), (2,0)]
         , [(0,1), (2,1), (0,2), (2,0)]
         , [(0,1), (1,2), (0,2), (2,0)]
         , [(0,1), (1,2), (2,1), (0,2), (2,0)]
         , [(0,1), (1,0), (1,2), (2,1), (0,2), (2,0)]
         ]
    make :: [(Int, Int)] -> Graph 'D () ()
    make xs = mkGraph (replicate 3 ()) $ zip xs $ repeat ()

triadCensus :: (Hashable v, Eq v, Read v) => Graph d v e -> [Int]
triadCensus gr = unsafePerformIO $ allocaVector $ \result -> do
    igraphTriadCensus (_graph gr) result
    map truncate <$> toList result

-- motifsRandesu

{#fun igraph_triad_census as ^ { `IGraph'
                               , castPtr `Ptr Vector' } -> `CInt' void- #}

{#fun igraph_motifs_randesu as ^ { `IGraph', castPtr `Ptr Vector', `Int'
                                 , castPtr `Ptr Vector' } -> `CInt' void- #}
