{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph
    ( LGraph(..)
    , U(..)
    , D(..)
    , Graph(..)
    , decodeC
    , empty
    , mkGraph
    , fromLabeledEdges
    , fromLabeledEdges'

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

import           Conduit
import           Control.Arrow             ((***))
import           Control.Monad             (forM, forM_, liftM, replicateM,
                                            unless)
import           Control.Monad.Primitive
import           Control.Monad.ST          (runST)
import qualified Data.ByteString           as B
import           Data.Conduit.Cereal
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.List                 (sortBy)
import           Data.Maybe
import           Data.Ord                  (comparing)
import           Data.Serialize
import           Foreign                   (with, castPtr)
import           System.IO.Unsafe          (unsafePerformIO)

import           IGraph.Internal.Attribute
import           IGraph.Internal.Constants
import           IGraph.Internal.Data
import           IGraph.Internal.Graph
import           IGraph.Internal.Selector
import           IGraph.Mutable
import           IGraph.Types

class MGraph d => Graph d where
    isDirected :: LGraph d v e -> Bool
    isD :: d -> Bool

    nNodes :: LGraph d v e -> Int
    nNodes (LGraph g _) = unsafePerformIO $ igraphVcount g
    {-# INLINE nNodes #-}

    nodes :: LGraph d v e -> [Int]
    nodes gr = [0 .. nNodes gr - 1]
    {-# INLINE nodes #-}

    nEdges :: LGraph d v e -> Int
    nEdges (LGraph g _) = unsafePerformIO $ igraphEcount g
    {-# INLINE nEdges #-}

    edges :: LGraph d v e -> [Edge]
    edges gr@(LGraph g _) = unsafePerformIO $ mapM (igraphEdge g) [0..n-1]
      where
        n = nEdges gr
    {-# INLINE edges #-}

    hasEdge :: LGraph d v e -> Edge -> Bool
    hasEdge (LGraph g _) (fr, to) = unsafePerformIO $ do
        i <- igraphGetEid g fr to True False
        return $ i >= 0
    {-# INLINE hasEdge #-}

    nodeLab :: Serialize v => LGraph d v e -> Node -> v
    nodeLab (LGraph g _) i = unsafePerformIO $
        igraphHaskellAttributeVAS g vertexAttr i >>= fromBS
    {-# INLINE nodeLab #-}

    nodeLabMaybe :: Serialize v => LGraph d v e -> Node -> Maybe v
    nodeLabMaybe gr@(LGraph g _) i = unsafePerformIO $ do
        x <- igraphHaskellAttributeHasAttr g IgraphAttributeVertex vertexAttr
        return $ if x
            then Just $ nodeLab gr i
            else Nothing
    {-# INLINE nodeLabMaybe #-}

    getNodes :: (Hashable v, Eq v) => LGraph d v e -> v -> [Node]
    getNodes gr x = M.lookupDefault [] x $ _labelToNode gr
    {-# INLINE getNodes #-}

    edgeLab :: Serialize e => LGraph d v e -> Edge -> e
    edgeLab (LGraph g _) (fr,to) = unsafePerformIO $
        igraphGetEid g fr to True True >>=
            igraphHaskellAttributeEAS g edgeAttr >>= fromBS
    {-# INLINE edgeLab #-}

    edgeLabMaybe :: Serialize e => LGraph d v e -> Edge -> Maybe e
    edgeLabMaybe gr@(LGraph g _) i = unsafePerformIO $ do
        x <- igraphHaskellAttributeHasAttr g IgraphAttributeEdge edgeAttr
        return $ if x
            then Just $ edgeLab gr i
            else Nothing
    {-# INLINE edgeLabMaybe #-}

    getEdgeByEid :: LGraph d v e -> Int -> Edge
    getEdgeByEid gr@(LGraph g _) i = unsafePerformIO $ igraphEdge g i
    {-# INLINE getEdgeByEid #-}

    edgeLabByEid :: Serialize e => LGraph d v e -> Int -> e
    edgeLabByEid (LGraph g _) i = unsafePerformIO $
        igraphHaskellAttributeEAS g edgeAttr i >>= fromBS
    {-# INLINE edgeLabByEid #-}

instance Graph U where
    isDirected = const False
    isD = const False

instance Graph D where
    isDirected = const True
    isD = const True

instance (Graph d, Serialize v, Serialize e, Hashable v, Eq v)
    => Serialize (LGraph d v e) where
        put gr = do
            put $ nNodes gr
            go (nodeLab gr) (nNodes gr) 0
            put $ nEdges gr
            go (\i -> (getEdgeByEid gr i, edgeLabByEid gr i)) (nEdges gr) 0
          where
            go f n i | i >= n = return ()
                     | otherwise = put (f i) >> go f n (i+1)
        get = do
            nn <- get
            nds <- replicateM nn get
            ne <- get
            es <- replicateM ne get
            return $ mkGraph nds es

decodeC :: ( PrimMonad m, MonadThrow m, Graph d
           , Serialize v, Serialize e, Hashable v, Eq v )
        => ConduitT B.ByteString o m (LGraph d v e)
decodeC = do
    nn <- sinkGet get
    nds <- replicateM nn $ sinkGet get
    ne <- sinkGet get
    conduitGet2 get .| deserializeGraph nds ne

empty :: (Graph d, Hashable v, Serialize v, Eq v, Serialize e)
      => LGraph d v e
empty = runST $ new 0 >>= unsafeFreeze

mkGraph :: (Graph d, Hashable v, Serialize v, Eq v, Serialize e)
        => [v] -> [LEdge e] -> LGraph d v e
mkGraph vattr es = runST $ do
    g <- new 0
    addLNodes vattr g
    addLEdges es g
    unsafeFreeze g
  where
    n = length vattr

fromLabeledEdges :: (Graph d, Hashable v, Serialize v, Eq v, Serialize e)
                 => [((v, v), e)] -> LGraph d v e
fromLabeledEdges es = mkGraph labels es'
  where
    es' = flip map es $ \((fr, to), x) -> ((f fr, f to), x)
      where f x = M.lookupDefault undefined x labelToId
    labels = S.toList $ S.fromList $ concat [ [a,b] | ((a,b),_) <- es ]
    labelToId = M.fromList $ zip labels [0..]

-- | Deserialize a graph.
fromLabeledEdges' :: (PrimMonad m, Graph d, Hashable v, Serialize v, Eq v, Serialize e)
                  => a    -- ^ Input, usually a file
                  -> (a -> ConduitT () ((v, v), e) m ())  -- ^ deserialize the input into a stream of edges
                  -> m (LGraph d v e)
fromLabeledEdges' input mkConduit = do
    (labelToId, _, ne) <- runConduit $ mkConduit input .|
        foldlC f (M.empty, 0::Int, 0::Int)
    let getId x = M.lookupDefault undefined x labelToId
    runConduit $ mkConduit input .|
        mapC (\((v1, v2), e) -> ((getId v1, getId v2), e)) .|
        deserializeGraph (fst $ unzip $ sortBy (comparing snd) $ M.toList labelToId) ne
  where
    f (vs, nn, ne) ((v1, v2), _) =
        let (vs', nn') = add v1 $ add v2 (vs, nn)
        in (vs', nn', ne+1)
      where
        add v (m, i) = if v `M.member` m
            then (m, i)
            else (M.insert v i m, i + 1)

deserializeGraph :: ( PrimMonad m, Graph d, Hashable v, Serialize v
                    , Eq v, Serialize e )
                 => [v]
                 -> Int   -- ^ The number of edges
                 -> ConduitT (LEdge e) o m (LGraph d v e)
deserializeGraph nds ne = do
    evec <- unsafePrimToPrim $ igraphVectorNew $ 2 * ne
    bsvec <- unsafePrimToPrim $ bsvectorNew ne
    let f i ((fr, to), attr) = unsafePrimToPrim $ do
            igraphVectorSet evec (i*2) $ fromIntegral fr
            igraphVectorSet evec (i*2+1) $ fromIntegral to
            unsafeUseAsCStringLen (encode attr) $ \bs -> with (BSLen bs) $ \ptr ->
                bsvectorSet bsvec i $ castPtr ptr
            return $ i + 1
    foldMC f 0
    gr@(MLGraph g) <- new 0
    addLNodes nds gr
    unsafePrimToPrim $ withEdgeAttr $ \eattr -> with (mkStrRec eattr bsvec) $ \ptr -> do
            vptr <- fromPtrs [castPtr ptr]
            withVectorPtr vptr (igraphAddEdges g evec . castPtr)
    unsafeFreeze gr
{-# INLINE deserializeGraph #-}

unsafeFreeze :: (Hashable v, Eq v, Serialize v, PrimMonad m)
             => MLGraph (PrimState m) d v e -> m (LGraph d v e)
unsafeFreeze (MLGraph g) = unsafePrimToPrim $ do
    nV <- igraphVcount g
    labels <- forM [0 .. nV - 1] $ \i ->
        igraphHaskellAttributeVAS g vertexAttr i >>= fromBS
    return $ LGraph g $ M.fromListWith (++) $ zip labels $ map return [0..nV-1]
  where

freeze :: (Hashable v, Eq v, Serialize v, PrimMonad m)
       => MLGraph (PrimState m) d v e -> m (LGraph d v e)
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
filterNodes :: (Hashable v, Eq v, Serialize v, Graph d)
            => (LGraph d v e -> Node -> Bool) -> LGraph d v e -> LGraph d v e
filterNodes f gr = runST $ do
    let deleted = filter (not . f gr) $ nodes gr
    gr' <- thaw gr
    delNodes deleted gr'
    unsafeFreeze gr'

-- | Apply a function to change nodes' labels.
mapNodes :: (Graph d, Serialize v1, Serialize v2, Hashable v2, Eq v2)
         => (Node -> v1 -> v2) -> LGraph d v1 e -> LGraph d v2 e
mapNodes f gr = runST $ do
    (MLGraph gptr) <- thaw gr
    let gr' = MLGraph gptr
    forM_ (nodes gr) $ \x -> setNodeAttr x (f x $ nodeLab gr x) gr'
    unsafeFreeze gr'

-- | Apply a function to change edges' labels.
mapEdges :: (Graph d, Serialize e1, Serialize e2, Hashable v, Eq v, Serialize v)
         => (Edge -> e1 -> e2) -> LGraph d v e1 -> LGraph d v e2
mapEdges f gr = runST $ do
    (MLGraph gptr) <- thaw gr
    let gr' = MLGraph gptr
    forM_ [0 .. nEdges gr - 1] $ \x -> do
        e <- unsafePrimToPrim $ igraphEdge (_graph gr) x
        setEdgeAttr x (f e $ edgeLabByEid gr x) gr'
    unsafeFreeze gr'


-- | Keep nodes that satisfy the constraint
filterEdges :: (Hashable v, Eq v, Serialize v, Graph d)
            => (LGraph d v e -> Edge -> Bool) -> LGraph d v e -> LGraph d v e
filterEdges f gr = runST $ do
    let deleted = filter (not . f gr) $ edges gr
    gr' <- thaw gr
    delEdges deleted gr'
    unsafeFreeze gr'

-- | Map a function over the node labels in a graph
nmap :: (Graph d, Serialize v, Hashable u, Serialize u, Eq u)
     => ((Node, v) -> u) -> LGraph d v e -> LGraph d u e
nmap fn gr = unsafePerformIO $ do
    (MLGraph g) <- thaw gr
    forM_ (nodes gr) $ \i -> do
        let label = fn (i, nodeLab gr i)
        asBS label $ \bs ->
            with bs (igraphHaskellAttributeVASSet g vertexAttr i)
    unsafeFreeze (MLGraph g)

-- | Map a function over the edge labels in a graph
emap :: (Graph d, Serialize v, Hashable v, Eq v, Serialize e1, Serialize e2)
     => ((Edge, e1) -> e2) -> LGraph d v e1 -> LGraph d v e2
emap fn gr = unsafePerformIO $ do
    (MLGraph g) <- thaw gr
    forM_ (edges gr) $ \(fr, to) -> do
        i <- igraphGetEid g fr to True True
        let label = fn ((fr,to), edgeLabByEid gr i)
        asBS label $ \bs ->
            with bs (igraphHaskellAttributeEASSet g edgeAttr i)
    unsafeFreeze (MLGraph g)
