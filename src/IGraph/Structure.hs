module IGraph.Structure
    ( inducedSubgraph
    , closeness
    , betweenness
    , eigenvectorCentrality
    , pagerank
    , personalizedPagerank
    ) where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)

import IGraph
import IGraph.Mutable
import IGraph.Internal.Graph
import IGraph.Internal.Data
import IGraph.Internal.Selector
import IGraph.Internal.Structure
import IGraph.Internal.Arpack
import IGraph.Internal.Constants
import IGraph.Internal.Attribute

inducedSubgraph :: (Hashable v, Eq v, Read v) => LGraph d v e -> [Int] -> LGraph d v e
inducedSubgraph gr vs = unsafePerformIO $ do
    vs' <- listToVector $ map fromIntegral vs
    vsptr <- igraphVsVector vs'
    mallocForeignPtrBytes 160 >>= \gptr -> withForeignPtr gptr $ \p -> do
        igraphInducedSubgraph (_graph gr) p vsptr IgraphSubgraphCreateFromScratch
        let g' = IGraphPtr gptr
            labToId = M.fromListWith (++) $ zip labels $ map return [0..nV-1]
            nV = igraphVcount g'
            labels = map (read . igraphCattributeVAS g' vertexAttr) [0 .. nV-1]
        return $ LGraph g' labToId

-- | closeness centrality
closeness :: [Int]  -- ^ vertices
          -> LGraph d v e
          -> Maybe [Double]  -- ^ optional edge weights
          -> Neimode
          -> Bool   -- ^ whether to normalize
          -> [Double]
closeness vs gr ws mode normal = unsafePerformIO $ do
    vs' <- listToVector $ map fromIntegral vs
    vsptr <- igraphVsVector vs'
    vptr <- igraphVectorNew 0
    ws' <- case ws of
        Just w -> listToVector w
        _ -> liftM VectorPtr $ newForeignPtr_ $ castPtr nullPtr
    igraphCloseness (_graph gr) vptr vsptr mode ws' normal
    vectorPtrToList vptr

-- | betweenness centrality
betweenness :: [Int]
            -> LGraph d v e
            -> Maybe [Double]
            -> [Double]
betweenness vs gr ws = unsafePerformIO $ do
    vs' <- listToVector $ map fromIntegral vs
    vsptr <- igraphVsVector vs'
    vptr <- igraphVectorNew 0
    ws' <- case ws of
        Just w -> listToVector w
        _ -> liftM VectorPtr $ newForeignPtr_ $ castPtr nullPtr
    igraphBetweenness (_graph gr) vptr vsptr True ws' False
    vectorPtrToList vptr

-- | eigenvector centrality
eigenvectorCentrality :: LGraph d v e
                      -> Maybe [Double]
                      -> [Double]
eigenvectorCentrality gr ws = unsafePerformIO $ do
    vptr <- igraphVectorNew 0
    ws' <- case ws of
        Just w -> listToVector w
        _ -> liftM VectorPtr $ newForeignPtr_ $ castPtr nullPtr
    arparck <- igraphArpackNew
    igraphEigenvectorCentrality (_graph gr) vptr nullPtr True True ws' arparck
    vectorPtrToList vptr

-- | Google's PageRank
pagerank :: Graph d
         => LGraph d v e
         -> Maybe [Double]  -- ^ edge weights
         -> Double  -- ^ damping factor, usually around 0.85
         -> [Double]
pagerank gr ws d = unsafePerformIO $ alloca $ \p -> do
    vptr <- igraphVectorNew 0
    vsptr <- igraphVsAll
    ws' <- case ws of
        Just w -> listToVector w
        _ -> liftM VectorPtr $ newForeignPtr_ $ castPtr nullPtr
    igraphPagerank (_graph gr) IgraphPagerankAlgoPrpack vptr p vsptr
        (isDirected gr) d ws' nullPtr
    vectorPtrToList vptr

personalizedPagerank :: Graph d
                     => LGraph d v e
                     -> [Double]   -- ^ reset probability
                     -> Maybe [Double]
                     -> Double
                     -> [Double]
personalizedPagerank gr reset ws d = unsafePerformIO $ alloca $ \p -> do
    vptr <- igraphVectorNew 0
    vsptr <- igraphVsAll
    ws' <- case ws of
        Just w -> listToVector w
        _ -> liftM VectorPtr $ newForeignPtr_ $ castPtr nullPtr
    reset' <- listToVector reset
    igraphPersonalizedPagerank (_graph gr) IgraphPagerankAlgoPrpack vptr p vsptr
        (isDirected gr) d reset' ws' nullPtr
    vectorPtrToList vptr
