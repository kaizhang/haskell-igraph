{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module IGraph
    ( LGraph(..)
    , U
    , D
    , Graph(..)
    , mkGraph
    , fromLabeledEdges

    , unsafeFreeze
    , unsafeThaw
    , thaw

    , neighbors
    , pre
    , suc
    ) where

import Control.Arrow ((***))
import Control.Monad (liftM)
import Control.Monad.ST (runST)
import Control.Monad.Primitive
import qualified Data.HashMap.Strict as M
import Data.List (nub)
import Data.Hashable (Hashable)
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

import IGraph.Mutable
import IGraph.Internal.Graph
import IGraph.Internal.Constants
import IGraph.Internal.Attribute
import IGraph.Internal.Selector

type Node = Int
type Edge = (Node, Node)

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph :: IGraphPtr
    , _labelToNode :: M.HashMap v [Node] }


class MGraph d => Graph d where
    nNodes :: LGraph d v e -> Int
    nNodes (LGraph g _) = igraphVcount g

    nEdges :: LGraph d v e -> Int
    nEdges (LGraph g _) = igraphEcount g

    edges :: LGraph d v e -> [Edge]
    edges gr@(LGraph g _) = unsafePerformIO $ mapM (igraphEdge g) [0..n-1]
      where
        n = nEdges gr

    nodeLab :: Read v => LGraph d v e -> Node -> v
    nodeLab (LGraph g _) i = read $ igraphCattributeVAS g vertexAttr i

    edgeLab :: Read e => LGraph d v e -> Edge -> e
    edgeLab (LGraph g _) (fr,to) = read $ igraphCattributeEAS g edgeAttr $
                                   igraphGetEid g fr to True True

    edgeLabByEid :: Read e => LGraph d v e -> Int -> e
    edgeLabByEid (LGraph g _) i = read $ igraphCattributeEAS g edgeAttr i


instance Graph U where
instance Graph D where


mkGraph :: (Graph d, Hashable v, Read v, Eq v, Show v, Show e)
        => (Node, Maybe [v]) -> ([Edge], Maybe [e]) -> LGraph d v e
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

fromLabeledEdges :: (Graph d, Hashable v, Read v, Eq v, Show v)
                 => [(v, v)] -> LGraph d v ()
fromLabeledEdges es = mkGraph (n, Just labels) (es', Nothing)
  where
    es' = map (f *** f) es
      where f x = M.lookupDefault undefined x labelToId
    labels = nub $ concat [ [a,b] | (a,b) <- es ]
    labelToId = M.fromList $ zip labels [0..]
    n = M.size labelToId

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

-- | Find all neighbors of the given node
neighbors :: LGraph d v e -> Node -> [Node]
neighbors gr i = unsafePerformIO $ do
    vs <- igraphVsNew
    igraphVsAdj vs i IgraphAll
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

-- | Find all nodes that have a link from the given node.
suc :: LGraph D v e -> Node -> [Node]
suc gr i = unsafePerformIO $ do
    vs <- igraphVsNew
    igraphVsAdj vs i IgraphOut
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

-- | Find all nodes that link to to the given node.
pre :: LGraph D v e -> Node -> [Node]
pre gr i = unsafePerformIO $ do
    vs <- igraphVsNew
    igraphVsAdj vs i IgraphIn
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

