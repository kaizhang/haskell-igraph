{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph
    ( Graph(..)
    , LGraph(..)
    , U
    , D
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

    , nmap
    , emap

    , nfilter
    , efilter
    ) where

import           Conduit
import           Control.Arrow             ((&&&))
import           Control.Monad             (forM, forM_, liftM, replicateM)
import           Control.Monad.Primitive
import           Control.Monad.ST          (runST)
import qualified Data.ByteString           as B
import           Data.Conduit.Cereal
import           Data.Either               (fromRight)
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.List                 (sortBy)
import           Data.Ord                  (comparing)
import           Data.Serialize
import           Foreign                   (castPtr)
import           System.IO.Unsafe          (unsafePerformIO)

import           IGraph.Internal
import           IGraph.Internal.Constants
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

    labNodes :: Serialize v => LGraph d v e -> [LNode v]
    labNodes gr = map (\i -> (i, nodeLab gr i)) $ nodes gr
    {-# INLINE labNodes #-}

    -- | Return the number of edges in a graph.
    nEdges :: LGraph d v e -> Int
    nEdges (LGraph g _) = unsafePerformIO $ igraphEcount g
    {-# INLINE nEdges #-}

    -- | Return all edges.
    edges :: LGraph d v e -> [Edge]
    edges gr = map (getEdgeByEid gr) [0 .. nEdges gr - 1]
    {-# INLINE edges #-}

    labEdges :: Serialize e => LGraph d v e -> [LEdge e]
    labEdges gr = map (getEdgeByEid gr &&& getEdgeLabByEid gr) [0 .. nEdges gr - 1]
    {-# INLINE labEdges #-}

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
    getEdgeByEid (LGraph g _) i = unsafePerformIO $ igraphEdge g i
    {-# INLINE getEdgeByEid #-}

    -- | Find the edge label by edge ID.
    getEdgeLabByEid :: Serialize e => LGraph d v e -> Int -> e
    getEdgeLabByEid (LGraph g _) i = unsafePerformIO $
        igraphHaskellAttributeEAS g edgeAttr i >>= bsToByteString >>=
            return . fromRight (error "decode failed") . decode
    {-# INLINE getEdgeLabByEid #-}

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
            go (\i -> (getEdgeByEid gr i, getEdgeLabByEid gr i)) (nEdges gr) 0
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
    _ <- foldMC f 0
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

-- | Apply a function to change nodes' labels.
nmap :: (Graph d, Serialize v1, Serialize v2, Hashable v2, Eq v2)
     => (LNode v1 -> v2) -> LGraph d v1 e -> LGraph d v2 e
nmap f gr = runST $ do
    (MLGraph gptr) <- thaw gr
    let gr' = MLGraph gptr
    forM_ (nodes gr) $ \x -> setNodeAttr x (f (x, nodeLab gr x)) gr'
    unsafeFreeze gr'

-- | Apply a function to change edges' labels.
emap :: (Graph d, Serialize e1, Serialize e2, Hashable v, Eq v, Serialize v)
     => (LEdge e1 -> e2) -> LGraph d v e1 -> LGraph d v e2
emap f gr = runST $ do
    (MLGraph gptr) <- thaw gr
    let gr' = MLGraph gptr
    forM_ [0 .. nEdges gr - 1] $ \i -> do
        let lab = f (getEdgeByEid gr i, getEdgeLabByEid gr i)
        setEdgeAttr i lab gr'
    unsafeFreeze gr'

-- | Keep nodes that satisfy the constraint.
nfilter :: (Hashable v, Eq v, Serialize v, Graph d)
        => (LNode v -> Bool) -> LGraph d v e -> LGraph d v e
nfilter f gr = runST $ do
    let deleted = fst $ unzip $ filter (not . f) $ labNodes gr
    gr' <- thaw gr
    delNodes deleted gr'
    unsafeFreeze gr'

-- | Keep edges that satisfy the constraint.
efilter :: (Hashable v, Eq v, Serialize v, Serialize e, Graph d)
        => (LEdge e -> Bool) -> LGraph d v e -> LGraph d v e
efilter f gr = runST $ do
    let deleted = fst $ unzip $ filter (not . f) $ labEdges gr
    gr' <- thaw gr
    delEdges deleted gr'
    unsafeFreeze gr'
