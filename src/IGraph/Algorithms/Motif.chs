{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
module IGraph.Algorithms.Motif
    ( dyadCensus
    , triad
    , triadCensus
    ) where

import System.IO.Unsafe (unsafePerformIO)

import Foreign

import IGraph
import IGraph.Internal.C2HS
{#import IGraph.Internal #}

#include "haskell_igraph.h"

-- | Dyad census means classifying each pair of vertices of a directed graph
-- into three categories: mutual, there is an edge from a to b and also
-- from b to a; asymmetric, there is an edge either from a to b or
-- from b to a but not the other way; null, no edges between a and b.
dyadCensus :: Graph D v e -> (Int, Int, Int)
dyadCensus = unsafePerformIO . igraphDyadCensus . _graph
{-# INLINE dyadCensus #-}

{#fun igraph_dyad_census as ^
    { `IGraph'
    , alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    } -> `CInt' void- #}

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
{-# INLINE triad #-}

-- | Calculating the triad census means classifying every triple of vertices
-- in a directed graph. A triple can be in one of 16 states listed in `triad`.
triadCensus :: (Ord v, Read v) => Graph D v e -> [Int]
triadCensus gr = unsafePerformIO $ allocaVector $ \result -> do
    igraphTriadCensus (_graph gr) result
    map truncate <$> toList result
{-# INLINE triadCensus #-}
{#fun igraph_triad_census as ^ { `IGraph'
                               , castPtr `Ptr Vector' } -> `CInt' void- #}

-- motifsRandesu
{#fun igraph_motifs_randesu as ^ { `IGraph', castPtr `Ptr Vector', `Int'
                                 , castPtr `Ptr Vector' } -> `CInt' void- #}
