{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IGraph
    ( Graph(..)
    , EdgeType(..)
    , Node
    , LNode
    , Edge
    , LEdge
    , isDirected
    , nNodes
    , nodeLab
    , nodes
    , labNodes
    , nEdges
    , edgeLab
    , edges
    , labEdges
    , addNodes
    , delNodes
    , addEdges
    , delEdges
    , hasEdge
    , getNodes
    , getEdgeByEid
    , getEdgeLabByEid
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

    -- * Non-simple graphs: multiple and loop edges
    , isSimple
    , hasMultiple
    ) where

import           Conduit
import           Control.Monad             (forM, forM_, replicateM, when)
import           Control.Monad.Primitive
import           Control.Monad.ST          (runST)
import           Data.Either               (fromRight)
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import           Data.List                 (sortBy)
import           Data.Ord                  (comparing)
import           Data.Serialize
import           Data.Primitive.MutVar
import           Data.Singletons           (Sing, SingI (..), fromSing)
import           Foreign                   (Ptr, castPtr)
import           System.IO.Unsafe          (unsafePerformIO)

import           IGraph.Internal
import           IGraph.Internal.Constants
import           IGraph.Mutable (MGraph(..))
import qualified IGraph.Mutable as GM
import           IGraph.Types

-- | Graph with labeled nodes and edges.
data Graph (d :: EdgeType) v e = Graph
    { _graph       :: IGraph
    , _labelToNode :: M.Map v [Node]
    }

instance (SingI d, Serialize v, Serialize e, Ord v)
    => Serialize (Graph d v e) where
        put gr = do
            put $ fromSing (sing :: Sing d)
            put $ nNodes gr
            go (nodeLab gr) (nNodes gr) 0
            put $ nEdges gr
            go (\i -> (getEdgeByEid gr i, getEdgeLabByEid gr i)) (nEdges gr) 0
          where
            go f n i | i >= n = return ()
                     | otherwise = put (f i) >> go f n (i+1)
        get = do
            directed <- get
            when (fromSing (sing :: Sing d) /= directed) $
                error "Incorrect graph type"
            nn <- get
            nds <- replicateM nn get
            ne <- get
            es <- replicateM ne get
            return $ mkGraph nds es

-- | Is the graph directed or not.
isDirected :: forall d v e. SingI d => Graph d v e -> Bool
isDirected _ = case fromSing (sing :: Sing d) of
    D -> True
    U -> False
{-# INLINE isDirected #-}

-- | Return the number of nodes in a graph.
nNodes :: Graph d v e -> Int
nNodes = unsafePerformIO . igraphVcount . _graph
{-# INLINE nNodes #-}

-- | Return all nodes. @nodes gr == [0 .. nNodes gr - 1]@.
nodes :: Graph d v e -> [Node]
nodes gr = [0 .. nNodes gr - 1]
{-# INLINE nodes #-}

labNodes :: Serialize v => Graph d v e -> [LNode v]
labNodes gr = map (\i -> (i, nodeLab gr i)) $ nodes gr
{-# INLINE labNodes #-}

-- | Return the number of edges in a graph.
nEdges :: Graph d v e -> Int
nEdges = unsafePerformIO . igraphEcount . _graph
{-# INLINE nEdges #-}

-- | Return all edges.
edges :: Graph d v e -> [Edge]
edges gr = map (getEdgeByEid gr) [0 .. nEdges gr - 1]
{-# INLINE edges #-}

labEdges :: Serialize e => Graph d v e -> [LEdge e]
labEdges gr = map (\i -> (getEdgeByEid gr i, getEdgeLabByEid gr i))
    [0 .. nEdges gr - 1]
{-# INLINE labEdges #-}

-- | Whether a edge exists in the graph.
hasEdge :: Edge -> Graph d v e -> Bool
hasEdge (fr, to) gr = unsafePerformIO $ do
    i <- igraphGetEid (_graph gr) fr to True False
    return $ i >= 0
{-# INLINE hasEdge #-}

-- | Return the label of given node.
nodeLab :: Serialize v => Graph d v e -> Node -> v
nodeLab gr i
    | i >= nNodes gr = error "Query node is not in the graph"
    | otherwise = unsafePerformIO $
        igraphHaskellAttributeVAS (_graph gr) vertexAttr i >>= toByteString >>=
            return . fromRight (error "decode failed") . decode
{-# INLINE nodeLab #-}

-- | Return all nodes that are associated with given label.
getNodes :: Ord v => Graph d v e -> v -> [Node]
getNodes gr x = M.findWithDefault [] x $ _labelToNode gr
{-# INLINE getNodes #-}

-- | Return the label of given edge.
edgeLab :: Serialize e => Graph d v e -> Edge -> e
edgeLab (Graph g _) (fr, to) = unsafePerformIO $
    igraphGetEid g fr to True True >>=
        igraphHaskellAttributeEAS g edgeAttr >>= toByteString >>=
            return . fromRight (error "decode failed") . decode
{-# INLINE edgeLab #-}

-- | Find the edge by edge ID.
getEdgeByEid :: Graph d v e -> Int -> Edge
getEdgeByEid (Graph g _) i = unsafePerformIO $ igraphEdge g i
{-# INLINE getEdgeByEid #-}

-- | Find the edge label by edge ID.
getEdgeLabByEid :: Serialize e => Graph d v e -> Int -> e
getEdgeLabByEid (Graph g _) i = unsafePerformIO $
    igraphHaskellAttributeEAS g edgeAttr i >>= toByteString >>=
        return . fromRight (error "decode failed") . decode
{-# INLINE getEdgeLabByEid #-}

-- | Add nodes with labels to the graph.
addNodes :: (Ord v, Serialize v)
         => [v]  -- ^ vertices' labels
         -> Graph d v e -> Graph d v e
addNodes nds gr = runST $ do
    gr' <- thaw gr
    GM.addNodes nds gr'
    unsafeFreeze gr'
{-# INLINE addNodes #-}

-- | Delete nodes from the graph.
delNodes :: (Ord v, Serialize v)
         => [Node] -> Graph d v e -> Graph d v e
delNodes nds gr = runST $ do
    gr' <- thaw gr
    GM.delNodes nds gr'
    unsafeFreeze gr'
{-# INLINE delNodes #-}

-- | Add edges with labels to the graph.
addEdges :: Serialize e
         => [LEdge e] -> Graph d v e -> Graph d v e
addEdges es gr = runST $ do
    gr' <- thaw gr
    GM.addEdges es gr'
    unsafeFreeze gr'
{-# INLINE addEdges #-}

-- | Delete edges from the graph.
delEdges :: SingI d => [Edge] -> Graph d v e -> Graph d v e
delEdges es gr = runST $ do
    gr' <- thaw gr
    GM.delEdges es gr'
    unsafeFreeze gr'
{-# INLINE delEdges #-}

-- | Create a empty graph.
empty :: (SingI d, Serialize v, Ord v, Serialize e)
      => Graph d v e
empty = runST $ GM.new [] >>= unsafeFreeze

-- | Create a graph.
mkGraph :: (SingI d, Serialize v, Ord v, Serialize e)
        => [v]        -- ^ Nodes. Each will be assigned a ID from 0 to N.
        -> [LEdge e]  -- ^ Labeled edges.
        -> Graph d v e
mkGraph vattr es = runST $ do
    g <- GM.new []
    GM.addNodes vattr g
    GM.addEdges es g
    unsafeFreeze g

-- | Create a graph from labeled edges.
fromLabeledEdges :: (SingI d, Serialize v, Ord v, Serialize e)
                 => [((v, v), e)] -> Graph d v e
fromLabeledEdges es = mkGraph labels es'
  where
    es' = flip map es $ \((fr, to), x) -> ((f fr, f to), x)
      where f x = M.findWithDefault undefined x labelToId
    labels = S.toList $ S.fromList $ concat [ [a,b] | ((a,b),_) <- es ]
    labelToId = M.fromList $ zip labels [0..]

-- | Create a graph from a stream of labeled edges.
fromLabeledEdges' :: (MonadUnliftIO m, SingI d, Serialize v, Ord v, Serialize e)
                  => a    -- ^ Input, usually a file
                  -> (a -> ConduitT () ((v, v), e) m ())  -- ^ deserialize the input into a stream of edges
                  -> m (Graph d v e)
fromLabeledEdges' input mkConduit = do
    (labelToId, _, ne) <- runConduit $ mkConduit input .|
        foldlC f (M.empty, 0::Int, 0::Int)
    let action evec bsvec = do
            let getId x = M.findWithDefault undefined x labelToId
            runConduit $ mkConduit input .|
                mapC (\((v1, v2), e) -> ((getId v1, getId v2), e)) .|
                deserializeGraph (fst $ unzip $ sortBy (comparing snd) $ M.toList labelToId) evec bsvec
    withRunInIO $ \runInIO -> allocaVectorN (2*ne) $ \evec ->
        allocaBSVectorN ne $ \bsvec -> (runInIO $ action evec bsvec)
  where
    f (vs, nn, ne) ((v1, v2), _) =
        let (vs', nn') = add v1 $ add v2 (vs, nn)
        in (vs', nn', ne+1)
      where
        add v (m, i) = if v `M.member` m
            then (m, i)
            else (M.insert v i m, i + 1)

deserializeGraph :: (MonadIO m, SingI d, Serialize v, Ord v, Serialize e)
                 => [v]
                 -> Ptr Vector  -- ^ a vector that is sufficient to hold all edges
                 -> Ptr BSVector
                 -> ConduitT (LEdge e) o m (Graph d v e)
deserializeGraph nds evec bsvec = do
    let f i ((fr, to), attr) = liftIO $ do
            igraphVectorSet evec (i*2) $ fromIntegral fr
            igraphVectorSet evec (i*2+1) $ fromIntegral to
            bsvectorSet bsvec i $ encode attr
            return $ i + 1
    _ <- foldMC f 0
    liftIO $ do
        gr <- GM.new []
        GM.addNodes nds gr
        withBSAttr edgeAttr bsvec $ \ptr ->
            withPtrs [ptr] (igraphAddEdges (_mgraph gr) evec . castPtr)
        unsafeFreeze gr
{-# INLINE deserializeGraph #-}

-- | Convert a mutable graph to immutable graph.
freeze :: PrimMonad m
       => MGraph (PrimState m) d v e -> m (Graph d v e)
freeze gr = do
    g' <- unsafePrimToPrim $ igraphCopy $ _mgraph gr
    readMutVar (_mlabelToNode gr) >>= return . Graph g'
{-# INLINE freeze #-}

-- | Convert a mutable graph to immutable graph. The original graph may not be
-- used afterwards.
unsafeFreeze :: PrimMonad m
             => MGraph (PrimState m) d v e -> m (Graph d v e)
unsafeFreeze (MGraph g l) = readMutVar l >>= return . Graph g
{-# INLINE unsafeFreeze #-}

-- | Create a mutable graph.
thaw :: PrimMonad m => Graph d v e -> m (MGraph (PrimState m) d v e)
thaw (Graph g l) = do
    g' <- unsafePrimToPrim $ igraphCopy g
    l' <- newMutVar l
    return $ MGraph g' l'
{-# INLINE thaw #-}

-- | Create a mutable graph. The original graph may not be used afterwards.
unsafeThaw :: PrimMonad m => Graph d v e -> m (MGraph (PrimState m) d v e)
unsafeThaw (Graph g l) = do
    l' <- newMutVar l
    return $ MGraph g l'
{-# INLINE unsafeThaw #-}

-- | Find all neighbors of the given node.
neighbors :: Graph d v e -> Node -> [Node]
neighbors gr i = unsafePerformIO $ withVerticesAdj i IgraphAll $ \vs ->
    iterateVerticesC (_graph gr) vs $ \source -> runConduit $ source .| sinkList

-- | Find all nodes that have a link from the given node.
suc :: Graph 'D v e -> Node -> [Node]
suc gr i = unsafePerformIO $ withVerticesAdj i IgraphOut $ \vs ->
    iterateVerticesC (_graph gr) vs $ \source -> runConduit $ source .| sinkList

-- | Find all nodes that link to to the given node.
pre :: Graph 'D v e -> Node -> [Node]
pre gr i = unsafePerformIO $ withVerticesAdj i IgraphIn $ \vs ->
    iterateVerticesC (_graph gr) vs $ \source -> runConduit $ source .| sinkList

-- | Apply a function to change nodes' labels.
nmap :: (Serialize v1, Serialize v2, Ord v2)
     => (LNode v1 -> v2) -> Graph d v1 e -> Graph d v2 e
nmap f gr = runST $ do
    gr' <- unsafePrimToPrim $ igraphCopy $ _graph gr
    labelToId <- fmap (M.fromListWith (++)) $ forM (nodes gr) $ \x -> do
        let l = f (x, nodeLab gr x)
        unsafePrimToPrim $ withByteString (encode l) $
            igraphHaskellAttributeVASSet gr' vertexAttr x
        return (l, [x])
    return $ Graph gr' labelToId

-- | Apply a function to change edges' labels.
emap :: (Serialize e1, Serialize e2, Ord v, Serialize v)
     => (LEdge e1 -> e2) -> Graph d v e1 -> Graph d v e2
emap f gr = runST $ do
    MGraph gptr l <- thaw gr
    let gr' = MGraph gptr l
    forM_ [0 .. nEdges gr - 1] $ \i -> do
        let lab = f (getEdgeByEid gr i, getEdgeLabByEid gr i)
        GM.setEdgeAttr i lab gr'
    unsafeFreeze gr'

-- | Keep nodes that satisfy the constraint.
nfilter :: (Ord v, Serialize v)
        => (LNode v -> Bool) -> Graph d v e -> Graph d v e
nfilter f gr = runST $ do
    let deleted = fst $ unzip $ filter (not . f) $ labNodes gr
    gr' <- thaw gr
    GM.delNodes deleted gr'
    unsafeFreeze gr'

-- | Keep edges that satisfy the constraint.
efilter :: (SingI d, Ord v, Serialize v, Serialize e)
        => (LEdge e -> Bool) -> Graph d v e -> Graph d v e
efilter f gr = runST $ do
    let deleted = fst $ unzip $ filter (not . f) $ labEdges gr
    gr' <- thaw gr
    GM.delEdges deleted gr'
    unsafeFreeze gr'

-- | Decides whether the input graph is a simple graph. A graph is a simple
-- graph if it does not contain loop edges and multiple edges.
isSimple :: Graph d v e -> Bool
isSimple = unsafePerformIO . igraphIsSimple . _graph
{-# INLINE isSimple #-}

-- | Check whether the graph has at least one multiple edge. An edge is a
-- multiple edge if there is another edge with the same head and tail vertices
-- in the graph.
hasMultiple :: Graph d v e -> Bool
hasMultiple = unsafePerformIO . igraphHasMultiple . _graph
{-# INLINE hasMultiple #-}
