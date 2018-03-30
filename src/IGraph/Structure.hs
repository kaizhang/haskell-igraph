module IGraph.Structure
    ( inducedSubgraph
    , closeness
    , betweenness
    , eigenvectorCentrality
    , pagerank
    , personalizedPagerank
    ) where

import           Control.Monad
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import           Data.Serialize            (Serialize)
import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe          (unsafePerformIO)

import           IGraph
import           IGraph.Internal.Arpack
import           IGraph.Internal.Attribute
import           IGraph.Internal.Constants
import           IGraph.Internal.Data
import           IGraph.Internal.Graph
import           IGraph.Internal.Selector
import           IGraph.Internal.Structure
import           IGraph.Mutable

inducedSubgraph :: (Hashable v, Eq v, Serialize v) => LGraph d v e -> [Int] -> LGraph d v e
inducedSubgraph gr vs = unsafePerformIO $ do
    vs' <- fromList $ map fromIntegral vs
    vsptr <- igraphVsVector vs'
    g' <- igraphInducedSubgraph (_graph gr) vsptr IgraphSubgraphCreateFromScratch
    nV <- igraphVcount g'
    labels <- forM [0 .. nV - 1] $ \i ->
        igraphHaskellAttributeVAS g' vertexAttr i >>= fromBS
    return $ LGraph g' $ M.fromListWith (++) $ zip labels $ map return [0..nV-1]

-- | closeness centrality
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

-- | betweenness centrality
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

-- | eigenvector centrality
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
