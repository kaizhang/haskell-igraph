{-# LANGUAGE DataKinds #-}
module Test.Basic
    ( tests
    ) where

import           Conduit
import           Control.Monad.ST
import           Data.List
import           Data.List.Ordered (nubSort)
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           IGraph
import qualified IGraph.Mutable    as GM

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
    gr = mkGraph (replicate 100 ()) $ zip edgeList $ repeat () :: Graph 'D () ()
    simple = mkGraph (replicate 3 ()) $ zip [(0,1),(1,2),(2,0)] $ repeat () :: Graph 'D () ()

graphCreationLabeled :: TestTree
graphCreationLabeled = testGroup "Graph creation -- with labels"
    [ testCase "" $ assertBool "" $ nNodes gr == n && nEdges gr == m
    , testCase "" $ edgeList @=? (sort $ map (\(fr,to) ->
        ((nodeLab gr fr, nodeLab gr to), edgeLab gr (fr, to))) $ edges gr)
    , testCase "" $ edgeList @=? (sort $ map (\(fr,to) ->
       ((nodeLab gr' fr, nodeLab gr' to), edgeLab gr' (fr, to))) $ edges gr')
    ]
  where
    edgeList = zip (sort $ map (\(a,b) -> (show a, show b)) $ unsafePerformIO $
        randEdges 10000 1000) $ repeat 1
    n = length $ nubSort $ concatMap (\((a,b),_) -> [a,b]) edgeList
    m = length edgeList
    gr = fromLabeledEdges edgeList :: Graph 'D String Int
    gr' = unsafePerformIO $ fromLabeledEdges' edgeList yieldMany :: Graph 'D String Int

graphEdit :: TestTree
graphEdit = testGroup "Graph editing"
    [ testCase "case 1" $ [((1,2), 'b')] @=? sort (getEdges simple')
    , testCase "case 2" $ [((0,2), 'c')] @=? sort (getEdges $ delNodes [1] simple)
    , testCase "case 3" $ 2 @=?
        (let gr = delNodes [1] simple in nodeLab gr $ head $ getNodes gr 2)
    , testCase "case 4" $ 4 @=?
        (let gr = addNodes [3,4,5] simple in nodeLab gr $ head $ getNodes gr 4)
    ]
  where
    simple = mkGraph [0,1,2] $
        [ ((0,1), 'a'), ((1,2), 'b'), ((0,2), 'c') ] :: Graph 'U Int Char
    simple' = runST $ do
        g <- thaw simple
        GM.delEdges [(0,1),(0,2)] g
        freeze g
    getEdges gr = map
        (\(a,b) -> ((nodeLab gr a, nodeLab gr b), edgeLab gr (a,b))) $ edges gr
