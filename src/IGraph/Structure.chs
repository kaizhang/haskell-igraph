{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Structure
    ( inducedSubgraph
    , closeness
    , betweenness
    , eigenvectorCentrality
    , pagerank
    , personalizedPagerank
    ) where

import           Control.Monad
import           Data.Either               (fromRight)
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import           Data.Serialize            (Serialize, decode)
import           System.IO.Unsafe          (unsafePerformIO)

import Foreign
import Foreign.C.Types

import           IGraph
import           IGraph.Mutable
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}

#include "igraph/igraph.h"

inducedSubgraph :: (Hashable v, Eq v, Serialize v) => LGraph d v e -> [Int] -> LGraph d v e
inducedSubgraph gr vs = unsafePerformIO $ do
    vs' <- fromList $ map fromIntegral vs
    vsptr <- igraphVsVector vs'
    igraphInducedSubgraph (_graph gr) vsptr IgraphSubgraphCreateFromScratch >>=
        unsafeFreeze . MLGraph

-- | Closeness centrality
closeness :: [Int]  -- ^ vertices
          -> LGraph d v e
          -> Maybe [Double]  -- ^ optional edge weights
          -> Neimode
          -> Bool   -- ^ whether to normalize
          -> [Double]
closeness vs gr ws mode normal = unsafePerformIO $ do
    vs' <- fromList $ map fromIntegral vs
    vsptr <- igraphVsVector vs'
    vptr <- igraphVectorNew 0
    ws' <- case ws of
        Just w -> fromList w
        _      -> liftM Vector $ newForeignPtr_ $ castPtr nullPtr
    igraphCloseness (_graph gr) vptr vsptr mode ws' normal
    toList vptr

-- | Betweenness centrality
betweenness :: [Int]
            -> LGraph d v e
            -> Maybe [Double]
            -> [Double]
betweenness vs gr ws = unsafePerformIO $ do
    vs' <- fromList $ map fromIntegral vs
    vsptr <- igraphVsVector vs'
    vptr <- igraphVectorNew 0
    ws' <- case ws of
        Just w -> fromList w
        _      -> liftM Vector $ newForeignPtr_ $ castPtr nullPtr
    igraphBetweenness (_graph gr) vptr vsptr True ws' False
    toList vptr

-- | Eigenvector centrality
eigenvectorCentrality :: LGraph d v e
                      -> Maybe [Double]
                      -> [Double]
eigenvectorCentrality gr ws = unsafePerformIO $ do
    vptr <- igraphVectorNew 0
    ws' <- case ws of
        Just w -> fromList w
        _      -> liftM Vector $ newForeignPtr_ $ castPtr nullPtr
    arparck <- igraphArpackNew
    igraphEigenvectorCentrality (_graph gr) vptr nullPtr True True ws' arparck
    toList vptr

-- | Google's PageRank
pagerank :: Graph d
         => LGraph d v e
         -> Maybe [Double]  -- ^ edge weights
         -> Double  -- ^ damping factor, usually around 0.85
         -> [Double]
pagerank gr ws d
    | n == 0 = []
    | otherwise = unsafePerformIO $ alloca $ \p -> do
        vptr <- igraphVectorNew 0
        vsptr <- igraphVsAll
        ws' <- case ws of
            Just w -> if length w /= m
                then error "pagerank: incorrect length of edge weight vector"
                else fromList w
            _ -> liftM Vector $ newForeignPtr_ $ castPtr nullPtr
        igraphPagerank (_graph gr) IgraphPagerankAlgoPrpack vptr p vsptr
            (isDirected gr) d ws' nullPtr
        toList vptr
  where
    n = nNodes gr
    m = nEdges gr

-- | Personalized PageRank.
personalizedPagerank :: Graph d
                     => LGraph d v e
                     -> [Double]   -- ^ reset probability
                     -> Maybe [Double]
                     -> Double
                     -> [Double]
personalizedPagerank gr reset ws d
    | n == 0 = []
    | length reset /= n = error "personalizedPagerank: incorrect length of reset vector"
    | otherwise = unsafePerformIO $ alloca $ \p -> do
        vptr <- igraphVectorNew 0
        vsptr <- igraphVsAll
        ws' <- case ws of
            Just w -> if length w /= m
                then error "pagerank: incorrect length of edge weight vector"
                else fromList w
            _ -> liftM Vector $ newForeignPtr_ $ castPtr nullPtr
        reset' <- fromList reset
        igraphPersonalizedPagerank (_graph gr) IgraphPagerankAlgoPrpack vptr p vsptr
            (isDirected gr) d reset' ws' nullPtr
        toList vptr
  where
    n = nNodes gr
    m = nEdges gr

{#fun igraph_induced_subgraph as ^
    { `IGraph'
    , allocaIGraph- `IGraph' addIGraphFinalizer*
    , %`IGraphVs'
    , `SubgraphImplementation'
    } -> `CInt' void- #}

{#fun igraph_closeness as ^ { `IGraph'
                            , `Vector'
                            , %`IGraphVs'
                            , `Neimode'
                            , `Vector'
                            , `Bool' } -> `CInt' void- #}

{#fun igraph_betweenness as ^ { `IGraph'
                              , `Vector'
                              , %`IGraphVs'
                              , `Bool'
                              , `Vector'
                              , `Bool' } -> `CInt' void- #}

{#fun igraph_eigenvector_centrality as ^ { `IGraph'
                                         , `Vector'
                                         , id `Ptr CDouble'
                                         , `Bool'
                                         , `Bool'
                                         , `Vector'
                                         , `ArpackOpt' } -> `CInt' void- #}

{#fun igraph_pagerank as ^
    { `IGraph'
    , `PagerankAlgo'
    , `Vector'
    , id `Ptr CDouble'
    , %`IGraphVs'
    , `Bool'
    , `Double'
    , `Vector'
    , id `Ptr ()'
    } -> `CInt' void- #}

{#fun igraph_personalized_pagerank as ^
    { `IGraph'
    , `PagerankAlgo'
    , `Vector'
    , id `Ptr CDouble'
    , %`IGraphVs'
    , `Bool'
    , `Double'
    , `Vector'
    , `Vector'
    , id `Ptr ()'
    } -> `CInt' void- #}
