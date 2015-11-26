module Test.Structure
    ( tests
    ) where

import Control.Arrow
import Control.Monad.ST
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils
import System.IO.Unsafe
import Data.List

import IGraph
import IGraph.Mutable
import IGraph.Structure

tests :: TestTree
tests = testGroup "Structure property tests"
    [ subGraphs
    ]

subGraphs :: TestTree
subGraphs = testGroup "generate induced subgraphs"
    [ testCase "" $ test case1 ]
  where
    case1 = ( [("a","b"), ("b","c"), ("c","a"), ("a","c")]
            , ["a","c"], [("a","c"), ("c","a")] )
    test (ori,ns,expect) = sort expect @=? sort result
      where
        gr = fromLabeledEdges ori :: LGraph D String ()
        ns' = map (head . getNodes gr) ns
        gr' = inducedSubgraph gr ns'
        result = map (nodeLab gr' *** nodeLab gr') $ edges gr'
