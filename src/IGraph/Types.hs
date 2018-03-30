
module IGraph.Types where

import qualified Data.HashMap.Strict   as M

import           IGraph.Internal.Graph

type Node = Int
type Edge = (Node, Node)
type LEdge a = (Int, Int, a)

data U = U
data D = D

-- | Mutable labeled graph
newtype MLGraph m d v e = MLGraph IGraphPtr

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph       :: IGraphPtr
    , _labelToNode :: M.HashMap v [Node]
    }
