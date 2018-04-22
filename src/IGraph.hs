{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph
    ( Graph(..)
    , LGraph(..)
    , U(..)
    , D(..)
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
import           Data.Either               (fromRight)
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.List                 (sortBy)
import           Data.Maybe
import           Data.Ord                  (comparing)
import           Data.Serialize
import           Foreign                   (castPtr, with)
import           System.IO.Unsafe          (unsafePerformIO)

import           IGraph.Internal.Attribute
import           IGraph.Internal.Constants
import           IGraph.Internal.Data
import           IGraph.Internal.Graph
import           IGraph.Internal.Selector
import           IGraph.Mutable
import           IGraph.Types

class MGraph d => Graph d where
    -- | Graph is directed or not.
    isDirected :: LGraph d v e -> Bool
    isD :: d -> Bool

    -- | Return the number of nodes in a graph.
    nNodes :: LGraph d v e -> Int
    nNodes (LGraph g _) = unsafePerformIO $ igraphVcount g
    {-# INLINE nNodes #-}

    -- | Return all nodes. @nodes gr == [0 .. nNodes gr - 1]@.
    nodes :: LGraph d v e -> [Node]
    nodes gr = [0 .. nNodes gr - 1]
    {-# INLINE nodes #-}

    -- | Return the number of edges in a graph.
    nEdges :: LGraph d v e -> Int
    nEdges (LGraph g _) = unsafePerformIO $ igraphEcount g
    {-# INLINE nEdges #-}

    -- | Return all edges.
    edges :: LGraph d v e -> [Edge]
    edges gr@(LGraph g _) = unsafePerformIO $ mapM (igraphEdge g) [0..n-1]
      where
        n = nEdges gr
    {-# INLINE edges #-}

    -- | Whether a edge exists in the graph.
    hasEdge :: LGraph d v e -> Edge -> Bool
    hasEdge (LGraph g _) (fr, to) = unsafePerformIO $ do
        i <- igraphGetEid g fr to True False
        return $ i >= 0
    {-# INLINE hasEdge #-}

    -- | Return the label of given node.
    nodeLab :: Serialize v => LGraph d v e -> Node -> v
    nodeLab (LGraph g _) i = unsafePerformIO $
        igraphHaskellAttributeVAS g vertexAttr i >>= bsToByteString >>=
            return . fromRight (error "decode failed") . decode
    {-# INLINE nodeLab #-}

    -- | Return all nodes that are associated with given label.
    getNodes :: (Hashable v, Eq v) => LGraph d v e -> v -> [Node]
    getNodes gr x = M.lookupDefault [] x $ _labelToNode gr
    {-# INLINE getNodes #-}

    -- | Return the label of given edge.
    edgeLab :: Serialize e => LGraph d v e -> Edge -> e
    edgeLab (LGraph g _) (fr,to) = unsafePerformIO $
        igraphGetEid g fr to True True >>=
            igraphHaskellAttributeEAS g edgeAttr >>= bsToByteString >>=
                return . fromRight (error "decode failed") . decode
    {-# INLINE edgeLab #-}

    -- | Find the edge by edge ID.
    getEdgeByEid :: LGraph d v e -> Int -> Edge
    getEdgeByEid gr@(LGraph g _) i = unsafePerformIO $ igraphEdge g i
    {-# INLINE getEdgeByEid #-}

    -- | Find the edge label by edge ID.
    edgeLabByEid :: Serialize e => LGraph d v e -> Int -> e
    edgeLabByEid (LGraph g _) i = unsafePerformIO $
        igraphHaskellAttributeEAS g edgeAttr i >>= bsToByteString >>=
            return . fromRight (error "decode failed") . decode
    {-# INLINE edgeLabByEid #-}

instance Graph U where
    isDirected = const False
    isD = const False

instance Graph D where
    isDirected = const True
    isD = const True

-- | Graph with labeled nodes and edges.
data LGraph d v e = LGraph
    { _graph       :: IGraph
    , _labelToNode :: M.HashMap v [Node]
    }

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

-- | Decode a graph from a stream of inputs. This may be more memory efficient
-- than standard @decode@ function.
decodeC :: ( PrimMonad m, MonadThrow m, Graph d
           , Serialize v, Serialize e, Hashable v, Eq v )
        => ConduitT B.ByteString o m (LGraph d v e)
decodeC = do
    nn <- sinkGet get
    nds <- replicateM nn $ sinkGet get
    ne <- sinkGet get
    conduitGet2 get .| deserializeGraph nds ne

-- | Create a empty graph.
empty :: (Graph d, Hashable v, Serialize v, Eq v, Serialize e)
      => LGraph d v e
empty = runST $ new 0 >>= unsafeFreeze

-- | Create a graph.
mkGraph :: (Graph d, Hashable v, Serialize v, Eq v, Serialize e)
        => [v]        -- ^ Nodes. Each will be assigned a ID from 0 to N.
        -> [LEdge e]  -- ^ Labeled edges.
        -> LGraph d v e
mkGraph vattr es = runST $ do
    g <- new 0
    addLNodes vattr g
    addLEdges es g
    unsafeFreeze g
  where
    n = length vattr

-- | Create a graph from labeled edges.
fromLabeledEdges :: (Graph d, Hashable v, Serialize v, Eq v, Serialize e)
                 => [((v, v), e)] -> LGraph d v e
fromLabeledEdges es = mkGraph labels es'
  where
    es' = flip map es $ \((fr, to), x) -> ((f fr, f to), x)
      where f x = M.lookupDefault undefined x labelToId
    labels = S.toList $ S.fromList $ concat [ [a,b] | ((a,b),_) <- es ]
    labelToId = M.fromList $ zip labels [0..]

-- | Create a graph from a stream of labeled edges.
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
            bsvectorSet bsvec i $ encode attr
            return $ i + 1
    foldMC f 0
    gr@(MLGraph g) <- new 0
    addLNodes nds gr
    unsafePrimToPrim $ withAttr edgeAttr bsvec $ \ptr -> do
            vptr <- fromPtrs [castPtr ptr]
            withVectorPtr vptr (igraphAddEdges g evec . castPtr)
    unsafeFreeze gr
{-# INLINE deserializeGraph #-}

-- | Convert a mutable graph to immutable graph.
freeze :: (Hashable v, Eq v, Serialize v, PrimMonad m)
       => MLGraph (PrimState m) d v e -> m (LGraph d v e)
freeze (MLGraph g) = do
    g' <- unsafePrimToPrim $ igraphCopy g
    unsafeFreeze (MLGraph g')

-- | Convert a mutable graph to immutable graph. The original graph may not be
-- used afterwards.
unsafeFreeze :: (Hashable v, Eq v, Serialize v, PrimMonad m)
             => MLGraph (PrimState m) d v e -> m (LGraph d v e)
unsafeFreeze (MLGraph g) = unsafePrimToPrim $ do
    nV <- igraphVcount g
    labels <- forM [0 .. nV - 1] $ \i ->
        igraphHaskellAttributeVAS g vertexAttr i >>= bsToByteString >>=
            return . fromRight (error "decode failed") . decode
    return $ LGraph g $ M.fromListWith (++) $ zip labels $ map return [0..nV-1]
  where

-- | Create a mutable graph.
thaw :: (PrimMonad m, Graph d) => LGraph d v e -> m (MLGraph (PrimState m) d v e)
thaw (LGraph g _) = unsafePrimToPrim . liftM MLGraph . igraphCopy $ g

-- | Create a mutable graph. The original graph may not be used afterwards.
unsafeThaw :: PrimMonad m => LGraph d v e -> m (MLGraph (PrimState m) d v e)
unsafeThaw (LGraph g _) = return $ MLGraph g

-- | Find all neighbors of the given node.
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

-- | Keep nodes that satisfy the constraint.
filterEdges :: (Hashable v, Eq v, Serialize v, Graph d)
            => (LGraph d v e -> Edge -> Bool) -> LGraph d v e -> LGraph d v e
filterEdges f gr = runST $ do
    let deleted = filter (not . f gr) $ edges gr
    gr' <- thaw gr
    delEdges deleted gr'
    unsafeFreeze gr'

-- | Map a function over the node labels in a graph.
nmap :: (Graph d, Serialize v, Hashable u, Serialize u, Eq u)
     => ((Node, v) -> u) -> LGraph d v e -> LGraph d u e
nmap fn gr = unsafePerformIO $ do
    (MLGraph g) <- thaw gr
    forM_ (nodes gr) $ \i -> do
        let label = fn (i, nodeLab gr i)
        asBS (encode label) (igraphHaskellAttributeVASSet g vertexAttr i)
    unsafeFreeze (MLGraph g)

-- | Map a function over the edge labels in a graph.
emap :: (Graph d, Serialize v, Hashable v, Eq v, Serialize e1, Serialize e2)
     => ((Edge, e1) -> e2) -> LGraph d v e1 -> LGraph d v e2
emap fn gr = unsafePerformIO $ do
    (MLGraph g) <- thaw gr
    forM_ (edges gr) $ \(fr, to) -> do
        i <- igraphGetEid g fr to True True
        let label = fn ((fr,to), edgeLabByEid gr i)
        asBS (encode label) (igraphHaskellAttributeEASSet g edgeAttr i)
    unsafeFreeze (MLGraph g)
