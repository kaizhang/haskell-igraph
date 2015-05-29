{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph where

import qualified Data.ByteString.Char8 as B
import Foreign hiding (new)

import IGraph.Internal.Graph
import IGraph.Internal.Initialization
import IGraph.Internal.Data
import IGraph.Internal.Attribute
import System.IO.Unsafe (unsafePerformIO)

data U
data D

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph :: IGraphPtr }

class Graph gr d where
    empty :: gr d v e
    empty = new 0

    new :: Int -> gr d v e

    addEdge :: (Int, Int) -> gr d v e -> IO ()

    addLEdges :: Show e => String -> [(Int, Int, e)] -> gr d v e -> IO ()


instance Graph LGraph U where
    new n = unsafePerformIO $ igraphInit >>= igraphNew n False >>= return . LGraph

    addEdge (fr,to) (LGraph g) = igraphAddEdge g fr to

    addLEdges name es (LGraph g) = do
        vec <- listToVector $ concat xs
        let attr = makeAttributeRecord name vs
        alloca $ \ptr -> do
            poke ptr attr
            vptr <- listToVectorP [castPtr ptr]
            igraphAddEdges g vec (castPtr vptr)
        return ()
      where
        (xs, vs) = unzip $ map ( \(a,b,v) -> ([fromIntegral a, fromIntegral b], v) ) es

