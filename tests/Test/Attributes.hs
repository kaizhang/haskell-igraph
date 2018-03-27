module Test.Attributes
    ( tests
    ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.List.Ordered         (nubSort)
import           Data.Maybe
import           Foreign
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           IGraph
import           IGraph.Internal.Attribute
import           IGraph.Mutable
import           IGraph.Structure

tests :: TestTree
tests = testGroup "Attribute tests"
    [ bsTest
    , nodeLabelTest
    , labelTest
    ]

bsTest :: TestTree
bsTest = testCase "BS" $ do
    let values = [1..10000] :: [Int]
    bs <- mapM unsafeToBS values
    values' <- forM bs $ \b -> with b $ \ptr -> fromBS ptr
    assertBool "" $ values == values'

nodeLabelTest :: TestTree
nodeLabelTest = testCase "node label test" $ do
    let ns = sort $ map show [38..7000]
        gr = mkGraph ns [] :: LGraph D String ()
    assertBool "" $ sort (map (nodeLab gr) $ nodes gr) == ns

labelTest :: TestTree
labelTest = testCase "edge label test" $ do
    dat <- randEdges 1000 10000
    let es = sort $ zipWith (\a b -> (a,b)) dat $ map show [1..]
        gr = fromLabeledEdges es :: LGraph D Int String
        es' = sort $ map (\(a,b) -> ((nodeLab gr a, nodeLab gr b), edgeLab gr (a,b))) $ edges gr
    assertBool "" $ es == es'
