module IGraph.Motif
    ( triad
    , triadCensus
    ) where

import Data.Hashable (Hashable)
import System.IO.Unsafe (unsafePerformIO)

import IGraph
import IGraph.Internal.Motif
import IGraph.Internal.Data

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
triad :: [LGraph D () ()]
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
    make :: [(Int, Int)] -> LGraph D () ()
    make xs = mkGraph (replicate 3 ()) $ zip xs $ repeat ()

triadCensus :: (Hashable v, Eq v, Read v) => LGraph d v e -> [Int]
triadCensus gr = unsafePerformIO $ do
    vptr <- igraphVectorNew 0
    igraphTriadCensus (_graph gr) vptr
    map truncate <$> toList vptr

-- motifsRandesu
