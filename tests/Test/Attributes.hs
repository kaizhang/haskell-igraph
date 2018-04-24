module Test.Attributes
    ( tests
    ) where

import           Conduit
import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.List.Ordered         (nubSort)
import           Data.Maybe
import           Data.Serialize
import           Foreign
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           IGraph
import           IGraph.Exporter.GEXF
import           IGraph.Internal
import           IGraph.Mutable
import           IGraph.Structure

tests :: TestTree
tests = testGroup "Attribute tests"
    [ nodeLabelTest
    , labelTest
    , serializeTest
    ]

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

serializeTest :: TestTree
serializeTest = testCase "serialize test" $ do
    dat <- randEdges 1000 10000
    let es = map ( \(a, b) -> (
            ( defaultNodeAttributes{_nodeZindex=a}
            , defaultNodeAttributes{_nodeZindex=b}), defaultEdgeAttributes) ) dat
        gr = fromLabeledEdges es :: LGraph D NodeAttr EdgeAttr
        gr' :: LGraph D NodeAttr EdgeAttr
        gr' = case decode $ encode gr of
            Left msg -> error msg
            Right r  -> r
        es' = map (\(a,b) -> ((nodeLab gr' a, nodeLab gr' b), edgeLab gr' (a,b))) $ edges gr'
    gr'' <- runConduit $ (yield $ encode gr) .| decodeC :: IO (LGraph D NodeAttr EdgeAttr)
    let es'' = map (\(a,b) -> ((nodeLab gr'' a, nodeLab gr'' b), edgeLab gr'' (a,b))) $ edges gr''
    assertBool "" $ sort (map show es) == sort (map show es') &&
        sort (map show es) == sort (map show es'')
