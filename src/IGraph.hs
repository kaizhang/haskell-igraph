{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module IGraph where

import Control.Monad (liftM)
import Control.Monad.ST (runST)
import Control.Monad.Primitive
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

import IGraph.Mutable
import IGraph.Internal.Graph
import IGraph.Internal.Constants
import IGraph.Internal.Attribute
import IGraph.Internal.Selector

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph :: IGraphPtr
    , _nodeLabelToId :: M.HashMap v [Int] }


class MGraph d => Graph d where
    nNodes :: LGraph d v e -> Int
    nNodes (LGraph g _) = igraphVcount g

    nEdges :: LGraph d v e -> Int
    nEdges (LGraph g _) = igraphEcount g

    nodeLab :: Read v => LGraph d v e -> Int -> v
    nodeLab (LGraph g _) i = read $ igraphCattributeVAS g vertexAttr i

    edgeLab :: Read e => LGraph d v e -> (Int, Int) -> e
    edgeLab (LGraph g _) (fr,to) = read $ igraphCattributeEAS g edgeAttr $ igraphGetEid g fr to True True

    edgeLabByEid :: Read e => LGraph d v e -> Int -> e
    edgeLabByEid (LGraph g _) i = read $ igraphCattributeEAS g edgeAttr i


instance Graph U where


mkGraph :: (Graph d, Hashable v, Read v, Eq v, Show v, Show e) => (Int, Maybe [v]) -> ([(Int, Int)], Maybe [e]) -> LGraph d v e
mkGraph (n, vattr) (es,eattr) = runST $ do
    g <- new 0
    let addV | isNothing vattr = addNodes n g
             | otherwise = addLNodes n (fromJust vattr) g
        addE | isNothing eattr = addEdges es g
             | otherwise = addLEdges (zip' es (fromJust eattr)) g
    addV
    addE
    unsafeFreeze g
  where
    zip' a b | length a /= length b = error "incorrect length"
             | otherwise = zipWith (\(x,y) z -> (x,y,z)) a b


unsafeFreeze :: (Hashable v, Eq v, Read v, PrimMonad m) => MLGraph (PrimState m) d v e -> m (LGraph d v e)
unsafeFreeze (MLGraph g) = return $ LGraph g labToId
  where
    labToId = M.fromListWith (++) $ zip labels $ map return [0..nV-1]
    nV = igraphVcount g
    labels = map (read . igraphCattributeVAS g vertexAttr) [0 .. nV-1]

unsafeThaw :: PrimMonad m => LGraph d v e -> m (MLGraph (PrimState m) d v e)
unsafeThaw (LGraph g _) = return $ MLGraph g

thaw :: (PrimMonad m, Graph d) => LGraph d v e -> m (MLGraph (PrimState m) d v e)
thaw (LGraph g _) = unsafePrimToPrim . liftM MLGraph . igraphCopy $ g

neighbors :: LGraph d v e -> Int -> [Int]
neighbors gr i = unsafePerformIO $ do
    vs <- igraphVsNew
    igraphVsAdj vs i IgraphAll
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

-- | Find all Nodes that have a link from the given Node.
suc :: LGraph D v e -> Int -> [Int]
suc gr i = unsafePerformIO $ do
    vs <- igraphVsNew
    igraphVsAdj vs i IgraphOut
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

-- | Find all Nodes that link to to the given Node.
pre :: LGraph D v e -> Int -> [Int]
pre gr i = unsafePerformIO $ do
    vs <- igraphVsNew
    igraphVsAdj vs i IgraphIn
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

