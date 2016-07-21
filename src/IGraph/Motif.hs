module IGraph.Motif
    (triad) where

import IGraph

-- | Every triple of vertices in a directed graph
-- 003: A, B, C, the empty graph.
-- 012: A->B, C, a graph with a single directed edge.
-- 102: A<->B, C, a graph with a mutual connection between two vertices.
-- 021D: A<-B->C, the binary out-tree.
-- 021U: A->B<-C, the binary in-tree.
-- 021C: A->B->C, the directed line.
-- 111D: A<->B<-C.
-- 111U: A<->B->C.
-- 030T: A->B<-C, A->C.
-- 030C: A<-B<-C, A->C.
-- 201: A<->B<->C.
-- 120D: A<-B->C, A<->C.
-- 120U: A->B<-C, A<->C.
-- 120C: A->B->C, A<->C.
-- 210: A->B<->C, A<->C.
-- 300: A<->B<->C, A<->C, the complete graph.
triad :: [LGraph D () ()]
triad = map make xs
  where
    xs = [ []
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
         , [(0,1), (1,2), (1,2), (2,1), (0,2), (2,0)]
         ]

make :: [(Int, Int)] -> LGraph D () ()
make xs = mkGraph (length xs, Nothing) (xs, Nothing)