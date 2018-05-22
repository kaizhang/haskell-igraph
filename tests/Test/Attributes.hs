{-# LANGUAGE DataKinds #-}
module Test.Attributes
    ( tests
    ) where

import           Data.List
import           Data.Serialize
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           IGraph
import           IGraph.Exporter.GEXF
import           IGraph.Internal
import           IGraph.Mutable

tests :: TestTree
tests = testGroup "Attribute tests"
    [ nodeLabelTest
    , labelTest
    , serializeTest
    ]

nodeLabelTest :: TestTree
nodeLabelTest = testCase "node label test" $ do
    let ns = sort $ map show [38..7000]
        gr = mkGraph ns [] :: Graph 'D String ()
    assertBool "" $ sort (map (nodeLab gr) $ nodes gr) == ns

labelTest :: TestTree
labelTest = testCase "edge label test" $ do
    dat <- randEdges 1000 10000
    let es = sort $ zipWith (\a b -> (a,b)) dat $ map show [1..]
        gr = fromLabeledEdges es :: Graph 'D Int String
        es' = sort $ map (\(a,b) -> ((nodeLab gr a, nodeLab gr b), edgeLab gr (a,b))) $ edges gr
    assertBool "" $ es == es'

serializeTest :: TestTree
serializeTest = testCase "serialize test" $ do
    dat <- randEdges 1000 10000
    let es = map ( \(a, b) -> (
            ( defaultNodeAttributes{_nodeLabel= show a}
            , defaultNodeAttributes{_nodeLabel= show b}), defaultEdgeAttributes) ) dat
        gr = fromLabeledEdges es :: Graph 'D NodeAttr EdgeAttr
        gr' :: Graph 'D NodeAttr EdgeAttr
        gr' = case decode $ encode gr of
            Left msg -> error msg
            Right r  -> r
        es' = map (\(a,b) -> ((nodeLab gr' a, nodeLab gr' b), edgeLab gr' (a,b))) $ edges gr'
    sort (map show es) @=? sort (map show es')
