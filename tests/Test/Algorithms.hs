{-# LANGUAGE DataKinds #-}
module Test.Algorithms
    ( tests
    ) where

import           Control.Arrow
import           Control.Monad.ST
import           Data.List
import qualified Data.Matrix.Unboxed as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           IGraph
import           IGraph.Random
import           IGraph.Algorithms
import qualified IGraph.Mutable      as GM

tests :: TestTree
tests = testGroup "Algorithms"
    [ graphIsomorphism
    , motifTest
    , cliqueTest
    , subGraphs
    , decomposeTest
    , pagerankTest
    ]

graphIsomorphism :: TestTree
graphIsomorphism = testCase "Graph isomorphism" $ assertBool "" $
    and (zipWith isomorphic triad triad) &&
    (not . or) (zipWith isomorphic triad $ reverse triad)

motifTest :: TestTree
motifTest = testGroup "Network motif"
    [ testCase "triad Census" $ M.toLists (M.ident 16 :: M.Matrix Int) @=?
        map triadCensus triad ]

cliqueTest :: TestTree
cliqueTest = testGroup "Clique"
    [ testCase "case 1" $ sort (map sort $ cliques gr (4,-1)) @=? c4
    , testCase "case 2" $ sort (map sort $ cliques gr (2,2)) @=? c2
    , testCase "case 3" $ sort (map sort $ largestCliques gr) @=? c4
    , testCase "case 4" $ sort (map sort $ cliques gr (-1,-1)) @=?
        sort (map sort $ c1 ++ c2 ++ c3 ++ c4)
    ]
  where
    gr = runST $ do
        g <- unsafeThaw (full 6 False :: Graph 'U () ())
        GM.delEdges [(0,1), (0,2), (3,5)] g
        unsafeFreeze g
    c1 = [[0], [1], [2], [3], [4], [5]]
    c2 = [ [0,3], [0,4], [0,5], [1,2], [1,3], [1,4], [1,5], [2,3], [2,4]
        , [2,5], [3,4], [4,5] ]
    c3 = [ [0,3,4], [0,4,5], [1,2,3], [1,2,4], [1,2,5], [1,3,4], [1,4,5],
        [2,3,4], [2,4,5] ]
    c4 = [[1, 2, 3, 4], [1, 2, 4, 5]]

subGraphs :: TestTree
subGraphs = testGroup "generate induced subgraphs"
    [ testCase "" $ test case1 ]
  where
    case1 = ( [("a","b"), ("b","c"), ("c","a"), ("a","c")]
            , ["a","c"], [("a","c"), ("c","a")] )
    test (ori,ns,expect) = sort expect @=? sort result
      where
        gr = fromLabeledEdges $ zip ori $ repeat () :: Graph 'D String ()
        ns' = map (head . getNodes gr) ns
        gr' = inducedSubgraph gr ns'
        result = map (nodeLab gr' *** nodeLab gr') $ edges gr'

decomposeTest :: TestTree
decomposeTest = testGroup "Decompose"
    [ testCase "ring" $ edges (head $ decompose $ ring 10) @?=
        [(0,1), (1,2), (2,3), (3,4), (4,5), (5,6), (6,7), (7,8), (8,9), (0,9)]
    , testCase "1 component" $ do
        gr <- (withSeed 1244 $ erdosRenyiGame (GNP 100 (40/100)) False) :: IO (Graph 'U () ())
        1 @?= length (decompose gr)
    , testCase "toy example" $ map (sort . edges) (decompose gr) @?=
        [ [(0,1), (0,2), (1,2)]
        , [(0,1), (1,2), (2,3)]
        , []
        , [(0,1), (1,2)] ]
    ]
  where
    es = [ (0,1), (1,2), (2,0)
		 , (3,4), (4,5), (5,6)
		 , (8,9), (9,10) ]
    gr = mkGraph (replicate 11 ()) $ zip es $ repeat () :: Graph 'U () ()

{-
communityTest :: TestTree
communityTest = testGroup "Community"
    [ consistency ]
  where
    consistency = testCase "Consistency" $ do
        r1 <- withSeed 134 $ return . findCommunity zacharyKarate Nothing spinglass
        r2 <- withSeed 14 $ return . findCommunity zacharyKarate Nothing spinglass 
        r1 @=? r2
        -}

pagerankTest :: TestTree
pagerankTest = testGroup "PageRank"
    [ consistency
    , testCase "case 1" $ ranks @=? ranks' ]
  where
    consistency = testCase "Consistency" $ 
        pagerank gr 0.85 Nothing Nothing @=?
        pagerank gr 0.85 Nothing Nothing
    gr = star 11
    ranks = [0.47,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05]
    ranks' = map ((/100) . fromIntegral . round. (*100)) $
        pagerank gr 0.85 Nothing Nothing
