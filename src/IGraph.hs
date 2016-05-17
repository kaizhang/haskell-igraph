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
    , freeze
    , unsafeThaw
    , thaw

    , neighbors
    , pre
    , suc

    , filterNode
    , filterEdge
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
    , _labelToNode :: M.HashMap v [Node]
    }

class MGraph d => Graph d where
    isDirected :: LGraph d v e -> Bool
    isD :: d -> Bool

    nNodes :: LGraph d v e -> Int
    nNodes (LGraph g _) = igraphVcount g
    {-# INLINE nNodes #-}

    nodes :: LGraph d v e -> [Int]
    nodes gr = [0 .. nNodes gr - 1]
    {-# INLINE nodes #-}

    nEdges :: LGraph d v e -> Int
    nEdges (LGraph g _) = igraphEcount g
    {-# INLINE nEdges #-}

    edges :: LGraph d v e -> [Edge]
    edges gr@(LGraph g _) = unsafePerformIO $ mapM (igraphEdge g) [0..n-1]
      where
        n = nEdges gr
    {-# INLINE edges #-}

    hasEdge :: LGraph d v e -> Edge -> Bool
    hasEdge (LGraph g _) (fr, to)
        | igraphGetEid g fr to True False < 0 = False
        | otherwise = True
    {-# INLINE hasEdge #-}

    nodeLab :: Read v => LGraph d v e -> Node -> v
    nodeLab (LGraph g _) i = read $ igraphCattributeVAS g vertexAttr i
    {-# INLINE nodeLab #-}

    nodeLabMaybe :: Read v => LGraph d v e -> Node -> Maybe v
    nodeLabMaybe gr@(LGraph g _) i =
        if igraphCattributeHasAttr g IgraphAttributeVertex vertexAttr
            then Just $ nodeLab gr i
            else Nothing
    {-# INLINE nodeLabMaybe #-}

    getNodes :: (Hashable v, Eq v) => LGraph d v e -> v -> [Node]
    getNodes gr x = M.lookupDefault [] x $ _labelToNode gr
    {-# INLINE getNodes #-}

    edgeLab :: Read e => LGraph d v e -> Edge -> e
    edgeLab (LGraph g _) (fr,to) = read $ igraphCattributeEAS g edgeAttr $
                                   igraphGetEid g fr to True True
    {-# INLINE edgeLab #-}

    edgeLabMaybe :: Read e => LGraph d v e -> Edge -> Maybe e
    edgeLabMaybe gr@(LGraph g _) i =
        if igraphCattributeHasAttr g IgraphAttributeEdge edgeAttr
            then Just $ edgeLab gr i
            else Nothing
    {-# INLINE edgeLabMaybe #-}

    edgeLabByEid :: Read e => LGraph d v e -> Int -> e
    edgeLabByEid (LGraph g _) i = read $ igraphCattributeEAS g edgeAttr i
    {-# INLINE edgeLabByEid #-}


instance Graph U where
    isDirected = const False
    isD = const False

instance Graph D where
    isDirected = const True
    isD = const True

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

freeze :: (Hashable v, Eq v, Read v, PrimMonad m) => MLGraph (PrimState m) d v e -> m (LGraph d v e)
freeze (MLGraph g) = do
    g' <- unsafePrimToPrim $ igraphCopy g
    unsafeFreeze (MLGraph g')

unsafeThaw :: PrimMonad m => LGraph d v e -> m (MLGraph (PrimState m) d v e)
unsafeThaw (LGraph g _) = return $ MLGraph g

thaw :: (PrimMonad m, Graph d) => LGraph d v e -> m (MLGraph (PrimState m) d v e)
thaw (LGraph g _) = unsafePrimToPrim . liftM MLGraph . igraphCopy $ g

-- | Find all neighbors of the given node
neighbors :: LGraph d v e -> Node -> [Node]
neighbors gr i = unsafePerformIO $ do
    vs <- igraphVsAdj i IgraphAll
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

-- | Find all nodes that have a link from the given node.
suc :: LGraph D v e -> Node -> [Node]
suc gr i = unsafePerformIO $ do
    vs <- igraphVsAdj i IgraphOut
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

-- | Find all nodes that link to to the given node.
pre :: LGraph D v e -> Node -> [Node]
pre gr i = unsafePerformIO $ do
    vs <- igraphVsAdj i IgraphIn
    vit <- igraphVitNew (_graph gr) vs
    vitToList vit

-- | Keep nodes that satisfy the constraint
filterNode :: (Hashable v, Eq v, Read v, Graph d)
           => (LGraph d v e -> Node -> Bool) -> LGraph d v e -> LGraph d v e
filterNode f gr = runST $ do
    let deleted = filter (not . f gr) $ nodes gr
    gr' <- thaw gr
    delNodes deleted gr'
    unsafeFreeze gr'

-- | Keep nodes that satisfy the constraint
filterEdge :: (Hashable v, Eq v, Read v, Graph d)
           => (LGraph d v e -> Edge -> Bool) -> LGraph d v e -> LGraph d v e
filterEdge f gr = runST $ do
    let deleted = filter (not . f gr) $ edges gr
    gr' <- thaw gr
    delEdges deleted gr'
    unsafeFreeze gr'
