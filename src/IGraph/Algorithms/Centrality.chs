{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Algorithms.Centrality
    ( closeness
    , betweenness
    , eigenvectorCentrality
    , pagerank
    ) where

import           Control.Monad
import           Data.Serialize            (Serialize)
import Data.List (foldl')
import           System.IO.Unsafe          (unsafePerformIO)
import Data.Maybe
import Data.Singletons (SingI)

import Foreign
import Foreign.C.Types

import           IGraph
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}

#include "haskell_igraph.h"

-- | The normalized closeness centrality of a node is the average length of the
-- shortest path between the node and all other nodes in the graph.
closeness :: Serialize e
          => [Int]  -- ^ vertices
          -> Graph d v e
          -> Bool   -- ^ whether to normalize the results
          -> Maybe (e -> Double)  -- ^ Function to get edge weights
          -> [Double]
closeness nds gr normal getEdgeW = unsafePerformIO $ allocaVector $ \result ->
    withVerticesList nds $ \vs -> withListMaybe ws $ \ws' -> do
        igraphCloseness (_graph gr) result vs IgraphOut ws' normal
        toList result
  where
    ws = case getEdgeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labEdges gr
{#fun igraph_closeness as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr %`Ptr VertexSelector'
    , `Neimode'
    , castPtr `Ptr Vector'
    , `Bool' } -> `CInt' void- #}


-- | Betweenness centrality
betweenness :: Serialize e
            => [Int]
            -> Graph d v e
            -> Maybe (e -> Double)  -- ^ Function to get edge weights
            -> [Double]
betweenness nds gr getEdgeW = unsafePerformIO $ allocaVector $ \result ->
    withVerticesList nds $ \vs -> withListMaybe ws $ \ws' -> do
        igraphBetweenness (_graph gr) result vs True ws' False
        toList result
  where
    ws = case getEdgeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labEdges gr
{#fun igraph_betweenness as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr %`Ptr VertexSelector'
    , `Bool'
    , castPtr `Ptr Vector'
    , `Bool' } -> `CInt' void- #}

-- | Eigenvector centrality
eigenvectorCentrality :: Serialize e
                      => Graph d v e
                      -> Maybe (e -> Double)  -- ^ Function to get edge weights
                      -> [Double]
eigenvectorCentrality gr getEdgeW = unsafePerformIO $ allocaArpackOpt $ \arparck ->
    allocaVector $ \result -> withListMaybe ws $ \ws' -> do
        igraphEigenvectorCentrality (_graph gr) result nullPtr True True ws' arparck
        toList result
  where
    ws = case getEdgeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labEdges gr
{#fun igraph_eigenvector_centrality as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , id `Ptr CDouble'
    , `Bool'
    , `Bool'
    , castPtr `Ptr Vector'
    , castPtr `Ptr ArpackOpt' } -> `CInt' void- #}

-- | Google's PageRank algorithm, with option to
pagerank :: (SingI d, Serialize v, Serialize e)
         => Graph d v e
         -> Double  -- ^ damping factor, usually around 0.85
         -> Maybe (v -> Double)  -- ^ Node weights or reset probability. If provided,
                                 -- the personalized PageRank will be used
         -> Maybe (e -> Double)  -- ^ Edge weights
         -> [Double]
pagerank gr d getNodeW getEdgeW
    | nNodes gr == 0 = []
    | fmap (foldl' (+) 0) reset == Just 0 = error "sum of node weight vector must be non-zero"
    | otherwise = unsafePerformIO $ alloca $ \p -> allocaVector $ \result ->
        withVerticesAll $ \vs -> withListMaybe ws $ \ws' -> do
            case reset of
                Nothing -> igraphPagerank (_graph gr) IgraphPagerankAlgoPrpack
                    result p vs (isDirected gr) d ws' nullPtr
                Just reset' -> withList reset' $ \reset'' -> igraphPersonalizedPagerank
                    (_graph gr) IgraphPagerankAlgoPrpack result p vs
                    (isDirected gr) d reset'' ws' nullPtr
            toList result
  where
    reset = case getNodeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labNodes gr
    ws = case getEdgeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labEdges gr
{#fun igraph_pagerank as ^
    { `IGraph'
    , `PagerankAlgo'
    , castPtr `Ptr Vector'
    , id `Ptr CDouble'
    , castPtr %`Ptr VertexSelector'
    , `Bool'
    , `Double'
    , castPtr `Ptr Vector'
    , id `Ptr ()'
    } -> `CInt' void- #}
{#fun igraph_personalized_pagerank as ^
    { `IGraph'
    , `PagerankAlgo'
    , castPtr `Ptr Vector'
    , id `Ptr CDouble'
    , castPtr %`Ptr VertexSelector'
    , `Bool'
    , `Double'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , id `Ptr ()'
    } -> `CInt' void- #}
