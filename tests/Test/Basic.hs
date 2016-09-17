module Test.Basic
    ( tests
    ) where

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
tests = testGroup "Basic tests"
    [ graphCreation
    , graphEdit
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
    gr = mkGraph (replicate 100 ()) $ zip edgeList $ repeat () :: LGraph D () ()
    simple = mkGraph (replicate 3 ()) $ zip [(0,1),(1,2),(2,0)] $ repeat () :: LGraph D () ()

graphEdit :: TestTree
graphEdit = testGroup "Graph editing"
    [ testCase "" $ [(1,2)] @=? (sort $ edges simple') ]
  where
    simple = mkGraph (replicate 3 ()) $ zip [(0,1),(1,2),(2,0)] $ repeat () :: LGraph U () ()
    simple' = runST $ do
        g <- thaw simple
        delEdges [(0,1),(0,2)] g
        freeze g
