
module IGraph.Types where

type Node = Int
type LNode a = (Node, a)

type Edge = (Node, Node)
type LEdge a = (Edge, a)

-- | Undirected graph.
data U

-- | Directed graph.
data D
