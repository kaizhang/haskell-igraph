{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph where

import Foreign hiding (new)
import Data.Maybe

import IGraph.Internal.Graph
import IGraph.Internal.Initialization
import IGraph.Internal.Data
import IGraph.Internal.Attribute
import System.IO.Unsafe (unsafePerformIO)

-- constants
vertexAttr :: String
vertexAttr = "vertex_attribute"

edgeAttr :: String
edgeAttr = "edge_attribute"

data U
data D

type LEdge a = (Int, Int, a)

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph :: IGraphPtr }

class Graph gr d where
    empty :: gr d v e
    empty = new 0

    new :: Int -> gr d v e

    mkGraph :: (Show v, Show e) => (Int, Maybe [v]) -> ([(Int, Int)], Maybe [e]) -> gr d v e
    mkGraph (n, vattr) (es,eattr) = unsafePerformIO $ do
        let g = empty
            addV | isNothing vattr = addVertices n g
                 | otherwise = addLVertices n (fromJust vattr) g
            addE | isNothing eattr = addEdges es g
                 | otherwise = addLEdges (zip' es (fromJust eattr)) g
        addV
        addE
        return g
      where
        zip' a b | length a /= length b = error "incorrect length"
                 | otherwise = zipWith (\(x,y) z -> (x,y,z)) a b

    vertexLab :: Read v => Int -> gr d v e -> v

    edgeLab :: Read e => (Int, Int) -> gr d v e -> e

    addVertices :: Int -> gr d v e -> IO ()

    addLVertices :: Show v
                 => Int  -- ^ the number of new vertices add to the graph
                 -> [v]  -- ^ vertices' labels
                 -> gr d v e -> IO ()

    addEdges :: [(Int, Int)] -> gr d v e -> IO ()

    addLEdges :: Show e => [LEdge e] -> gr d v e -> IO ()


instance Graph LGraph U where
    new n = unsafePerformIO $ igraphInit >>= igraphNew n False >>= return . LGraph

    vertexLab i (LGraph g) = read $ igraphCattributeVAS g vertexAttr i

    edgeLab (fr,to) (LGraph g) = read $ igraphCattributeEAS g edgeAttr $ igraphGetEid g fr to True True

    addVertices n (LGraph g) = igraphAddVertices g n nullPtr

    addLVertices n labels (LGraph g)
        | n /= length labels = error "addLVertices: incorrect number of labels"
        | otherwise = do
            let attr = makeAttributeRecord vertexAttr labels
            alloca $ \ptr -> do
                poke ptr attr
                vptr <- listToVectorP [castPtr ptr]
                igraphAddVertices g n (castPtr vptr)

    addEdges es (LGraph g) = do
        vec <- listToVector xs
        igraphAddEdges g vec nullPtr
      where
        xs = concatMap ( \(a,b) -> [fromIntegral a, fromIntegral b] ) es

    addLEdges es (LGraph g) = do
        vec <- listToVector $ concat xs
        let attr = makeAttributeRecord edgeAttr vs
        alloca $ \ptr -> do
            poke ptr attr
            vptr <- listToVectorP [castPtr ptr]
            igraphAddEdges g vec (castPtr vptr)
      where
        (xs, vs) = unzip $ map ( \(a,b,v) -> ([fromIntegral a, fromIntegral b], v) ) es

