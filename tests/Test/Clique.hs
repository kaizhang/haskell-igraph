{-# LANGUAGE DataKinds #-}
module Test.Clique
    ( tests
    ) where

import           Control.Monad.ST
import           Data.List
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           IGraph
import           IGraph.Clique
import           IGraph.Generators
import           IGraph.Mutable

tests :: TestTree
tests = testGroup "Clique"
    [ testCase "case 1" $ sort (map sort $ cliques gr (4,-1)) @=? c4
    , testCase "case 2" $ sort (map sort $ cliques gr (2,2)) @=? c2
    , testCase "case 3" $ sort (map sort $ largestCliques gr) @=? c4
    , testCase "case 4" $ sort (map sort $ cliques gr (-1,-1)) @=?
        sort (map sort $ c1 ++ c2 ++ c3 ++ c4)
    ]
  where
    gr = runST $ do
        g <- unsafeThaw (full 6 False :: Graph 'U () ())
        delEdges [(0,1), (0,2), (3,5)] g
        unsafeFreeze g
    c1 = [[0], [1], [2], [3], [4], [5]]
    c2 = [ [0,3], [0,4], [0,5], [1,2], [1,3], [1,4], [1,5], [2,3], [2,4]
        , [2,5], [3,4], [4,5] ]
    c3 = [ [0,3,4], [0,4,5], [1,2,3], [1,2,4], [1,2,5], [1,3,4], [1,4,5],
        [2,3,4], [2,4,5] ]
    c4 = [[1, 2, 3, 4], [1, 2, 4, 5]]
