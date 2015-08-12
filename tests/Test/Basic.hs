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
    gr = mkGraph (100,Nothing) (edgeList, Nothing) :: LGraph D () ()
    simple = mkGraph (3,Nothing) ([(0,1),(1,2),(2,0)],Nothing) :: LGraph D () ()

graphEdit :: TestTree
graphEdit = testGroup "Graph editing"
    [ testCase "" $ [(1,2)] @=? (sort $ edges simple') ]
  where
    simple = mkGraph (3,Nothing) ([(0,1),(1,2),(2,0)],Nothing) :: LGraph U () ()
    simple' = runST $ do
        g <- thaw simple
        delEdges [(0,1),(0,2)] g
        freeze g
