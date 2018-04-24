
module IGraph.Types where

import qualified Data.HashMap.Strict   as M

type Node = Int
type Edge = (Node, Node)
type LEdge a = (Edge, a)

-- | Undirected graph.
data U

-- | Directed graph.
data D
