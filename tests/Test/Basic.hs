module Test.Basic
    ( tests
    ) where

import           Control.Monad.ST
import           Data.List
import           Data.List.Ordered (nubSort)
import           Data.Maybe
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           IGraph
import           IGraph.Mutable
import           IGraph.Structure

tests :: TestTree
tests = testGroup "Basic tests"
    [ graphCreation
    , graphCreationLabeled
    , graphEdit
    ]

graphCreation :: TestTree
graphCreation = testGroup "Graph creation"
    [ testCase "" $ assertBool "" $ nNodes simple == 3 && nEdges simple == 3
    , testCase "" $ [(0,1),(1,2),(2,0)] @=? (sort $ edges simple)
    , testCase "" $ assertBool "" $ nNodes gr == 100 && nEdges gr == m
    , testCase "" $ edgeList @=? (sort $ edges gr)
    ]
  where
    edgeList = sort $ unsafePerformIO $ randEdges 1000 100
    m = length edgeList
    gr = mkGraph (replicate 100 ()) $ zip edgeList $ repeat () :: LGraph D () ()
    simple = mkGraph (replicate 3 ()) $ zip [(0,1),(1,2),(2,0)] $ repeat () :: LGraph D () ()

graphCreationLabeled :: TestTree
graphCreationLabeled = testGroup "Graph creation -- with labels"
    [ testCase "" $ assertBool "" $ nNodes gr == n && nEdges gr == m
    , testCase "" $ edgeList @=? (sort $ map (\(fr,to) ->
        (nodeLab gr fr, nodeLab gr to)) $ edges gr)
    ]
  where
    edgeList = sort $ map (\(a,b) -> (show a, show b)) $ unsafePerformIO $
        randEdges 10000 1000
    n = length $ nubSort $ concatMap (\(a,b) -> [a,b]) edgeList
    m = length edgeList
    gr = fromLabeledEdges $ zip edgeList $ repeat () :: LGraph D String ()

graphEdit :: TestTree
graphEdit = testGroup "Graph editing"
    [ testCase "" $ [(1,2)] @=? (sort $ edges simple') ]
  where
    simple = mkGraph (replicate 3 ()) $ zip [(0,1),(1,2),(2,0)] $ repeat () :: LGraph U () ()
    simple' = runST $ do
        g <- thaw simple
        delEdges [(0,1),(0,2)] g
        freeze g
