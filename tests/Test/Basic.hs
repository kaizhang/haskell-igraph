module Test.Basic
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils
import System.IO.Unsafe
import Data.List

import IGraph

tests :: TestTree
tests = testGroup "Basic tests"
    [ graphCreation
    ]

graphCreation :: TestTree
graphCreation = testGroup "Graph creation"
    [ testCase "" $ assertBool "" $ nNodes simple == 3 && nEdges simple == 3
    , testCase "" $ [(0,1),(1,2),(2,0)] @=? (sort $ edges simple)
    , testCase "" $ assertBool "" $ nNodes gr == 100 && nEdges gr == 1000
    , testCase "" $ edgeList @=? (sort $ edges gr)
    ]
  where
    edgeList = sort $ unsafePerformIO $ randEdges 1000 100
    gr = mkGraph (100,Nothing) (edgeList, Nothing) :: LGraph D () ()
    simple = mkGraph (3,Nothing) ([(0,1),(1,2),(2,0)],Nothing) :: LGraph D () ()
