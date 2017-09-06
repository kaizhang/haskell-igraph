{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph
    ( LGraph(..)
    , U(..)
    , D(..)
    , Graph(..)
    , empty
    , mkGraph
    , fromLabeledEdges

    , unsafeFreeze
    , freeze
    , unsafeThaw
    , thaw

    , neighbors
    , pre
    , suc

    , mapNodes
    , mapEdges
    , filterNodes
    , filterEdges

    , nmap
    , emap
    ) where

import           Control.Arrow             ((***))
import           Control.Monad             (forM_, liftM)
import           Control.Monad.Primitive
import           Control.Monad.ST          (runST)
import           Data.Binary
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet as S
import           Data.Maybe
import           System.IO.Unsafe          (unsafePerformIO)

import           IGraph.Internal.Attribute
import           IGraph.Internal.Constants
import           IGraph.Internal.Graph
import           IGraph.Internal.Selector
import           IGraph.Mutable

type Node = Int
type Edge = (Node, Node)

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph       :: IGraphPtr
    , _labelToNode :: M.HashMap v [Node]
    }

instance ( Binary v, Hashable v, Read v, Show v, Eq v
         , Binary e, Read e, Show e, Graph d) => Binary (LGraph d v e) where
    put gr = do
        put nlabs
        put es
        put elabs
      where
        nlabs = map (nodeLab gr) $ nodes gr
        es = edges gr
        elabs = map (edgeLab gr) es
    get = do
        nlabs <- get
        es <- get
        elabs <- get
        return $ mkGraph nlabs $ zip es elabs

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

empty :: (Graph d, Hashable v, Read v, Eq v, Show v, Show e)
      => LGraph d v e
empty = runST $ new 0 >>= unsafeFreeze

mkGraph :: (Graph d, Hashable v, Read v, Eq v, Show v, Show e)
        => [v] -> [(Edge, e)] -> LGraph d v e
mkGraph vattr es = runST $ do
    g <- new 0
    addLNodes n vattr g
    addLEdges (map (\((fr,to),x) -> (fr,to,x)) es) g
    unsafeFreeze g
  where
    n = length vattr

fromLabeledEdges :: (Graph d, Hashable v, Read v, Eq v, Show v, Show e)
                 => [((v, v), e)] -> LGraph d v e
fromLabeledEdges es = mkGraph labels es'
  where
    es' = flip map es $ \((fr, to), x) -> ((f fr, f to), x)
      where f x = M.lookupDefault undefined x labelToId
    labels = S.toList $ S.fromList $ concat [ [a,b] | ((a,b),_) <- es ]
    labelToId = M.fromList $ zip labels [0..]

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
filterNodes :: (Hashable v, Eq v, Read v, Graph d)
            => (LGraph d v e -> Node -> Bool) -> LGraph d v e -> LGraph d v e
filterNodes f gr = runST $ do
    let deleted = filter (not . f gr) $ nodes gr
    gr' <- thaw gr
    delNodes deleted gr'
    unsafeFreeze gr'

-- | Apply a function to change nodes' labels.
mapNodes :: (Graph d, Read v1, Show v2, Hashable v2, Eq v2, Read v2)
         => (Node -> v1 -> v2) -> LGraph d v1 e -> LGraph d v2 e
mapNodes f gr = runST $ do
    (MLGraph gptr) <- thaw gr
    let gr' = MLGraph gptr
    forM_ (nodes gr) $ \x -> setNodeAttr x (f x $ nodeLab gr x) gr'
    unsafeFreeze gr'

-- | Apply a function to change edges' labels.
mapEdges :: (Graph d, Read e1, Show e2, Hashable v, Eq v, Read v)
         => (Edge -> e1 -> e2) -> LGraph d v e1 -> LGraph d v e2
mapEdges f gr = runST $ do
    (MLGraph gptr) <- thaw gr
    let gr' = MLGraph gptr
    forM_ [0 .. nEdges gr - 1] $ \x -> do
        e <- unsafePrimToPrim $ igraphEdge (_graph gr) x
        setEdgeAttr x (f e $ edgeLabByEid gr x) gr'
    unsafeFreeze gr'


-- | Keep nodes that satisfy the constraint
filterEdges :: (Hashable v, Eq v, Read v, Graph d)
            => (LGraph d v e -> Edge -> Bool) -> LGraph d v e -> LGraph d v e
filterEdges f gr = runST $ do
    let deleted = filter (not . f gr) $ edges gr
    gr' <- thaw gr
    delEdges deleted gr'
    unsafeFreeze gr'

-- | Map a function over the node labels in a graph
nmap :: (Graph d, Read v, Hashable u, Read u, Eq u, Show u)
     => ((Node, v) -> u) -> LGraph d v e -> LGraph d u e
nmap fn gr = unsafePerformIO $ do
    (MLGraph g) <- thaw gr
    forM_ (nodes gr) $ \i -> do
        let label = fn (i, nodeLab gr i)
        igraphCattributeVASSet g vertexAttr i (show label)
    unsafeFreeze (MLGraph g)

-- | Map a function over the edge labels in a graph
emap :: (Graph d, Read v, Hashable v, Eq v, Read e1, Show e2)
     => ((Edge, e1) -> e2) -> LGraph d v e1 -> LGraph d v e2
emap fn gr = unsafePerformIO $ do
    (MLGraph g) <- thaw gr
    forM_ (edges gr) $ \(fr, to) -> do
        let label = fn ((fr,to), edgeLabByEid gr i)
            i = igraphGetEid g fr to True True
        igraphCattributeEASSet g edgeAttr i (show label)
    unsafeFreeze (MLGraph g)
