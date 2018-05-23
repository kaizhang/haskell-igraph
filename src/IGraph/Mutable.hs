{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IGraph.Mutable
    ( MGraph(..)
    , new
    , nNodes
    , nEdges
    , addNodes
    , delNodes
    , addEdges
    , delEdges
    , setEdgeAttr
    , setNodeAttr
    )where

import           Control.Monad                  (forM)
import           Control.Monad.Primitive
import           Data.Either                    (fromRight)
import Data.Serialize (decode)
import qualified Data.Map.Strict            as M
import           Data.List                      (foldl', delete)
import           Data.Primitive.MutVar
import           Data.Serialize                 (Serialize, encode)
import           Data.Singletons.Prelude        (Sing, SingI, fromSing, sing)
import           Foreign                        hiding (new)

import           IGraph.Internal
import           IGraph.Internal.Initialization
import           IGraph.Types

-- | Mutable labeled graph.
data MGraph m (d :: EdgeType) v e = MGraph
    { _mgraph       :: IGraph
    , _mlabelToNode :: MutVar m (M.Map v [Node])
    }

-- | Create a new graph.
new :: forall m d v e. (SingI d, Ord v, Serialize v, PrimMonad m)
    => [v] -> m (MGraph (PrimState m) d v e)
new nds = do
    gr <- unsafePrimToPrim $ do
        gr <- igraphInit >>= igraphNew n directed
        withAttr vertexAttr nds $ \attr ->
            withPtrs [attr] (igraphAddVertices gr n . castPtr)
        return gr
    m <- newMutVar $ M.fromListWith (++) $ zip nds $ map return [0 .. n - 1]
    return $ MGraph gr m
  where
    n = length nds
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False

-- | Return the number of nodes in a graph.
nNodes :: PrimMonad m => MGraph (PrimState m) d v e -> m Int
nNodes gr = unsafePrimToPrim $ igraphVcount $ _mgraph gr
{-# INLINE nNodes #-}

-- | Return the number of edges in a graph.
nEdges :: PrimMonad m => MGraph (PrimState m) d v e -> m Int
nEdges gr = unsafePrimToPrim $ igraphEcount $ _mgraph gr
{-# INLINE nEdges #-}

-- | Add nodes with labels to the graph.
addNodes :: (Ord v, Serialize v, PrimMonad m)
         => [v]  -- ^ vertices' labels
         -> MGraph (PrimState m) d v e -> m ()
addNodes labels gr = do
    m <- nNodes gr
    unsafePrimToPrim $ withAttr vertexAttr labels $ \attr ->
        withPtrs [attr] (igraphAddVertices (_mgraph gr) n . castPtr)
    modifyMutVar' (_mlabelToNode gr) $ \x ->
        foldl' (\acc (k,v) -> M.insertWith (++) k v acc) x $
            zip labels $ map return [m .. m + n - 1]
  where
    n = length labels
{-# INLINE addNodes #-}

-- | Return the label of given node.
nodeLab :: (PrimMonad m, Serialize v) => MGraph (PrimState m) d v e -> Node -> m v
nodeLab gr i = unsafePrimToPrim $
    igraphHaskellAttributeVAS (_mgraph gr) vertexAttr i >>= toByteString >>=
        return . fromRight (error "decode failed") . decode
{-# INLINE nodeLab #-}

-- | Delete nodes from the graph.
delNodes :: (PrimMonad m, Ord v, Serialize v)
         => [Node] -> MGraph (PrimState m) d v e -> m ()
delNodes ns gr = do
    unsafePrimToPrim $ withVerticesList ns $ igraphDeleteVertices (_mgraph gr)
    writeMutVar (_mlabelToNode gr) $ mkLabelToId $ _mgraph gr
{-# INLINE delNodes #-}

-- | Add edges with labels to the graph.
-- If you also want to add new vertices, call addNodes first.
addEdges :: (PrimMonad m, Serialize e)
         => [LEdge e] -> MGraph (PrimState m) d v e -> m ()
addEdges es gr = unsafePrimToPrim $
    withAttr edgeAttr vs $ \attr -> withList (concat xs) $ \vec ->
        withPtrs [attr] (igraphAddEdges (_mgraph gr) vec . castPtr)
  where
    (xs, vs) = unzip $ map ( \((a,b),v) -> ([a, b], v) ) es
{-# INLINE addEdges #-}

-- | Delete edges from the graph.
delEdges :: forall m d v e. (SingI d, PrimMonad m)
         => [Edge] -> MGraph (PrimState m) d v e -> m ()
delEdges es gr = unsafePrimToPrim $ do
    eids <- forM es $ \(fr, to) -> igraphGetEid (_mgraph gr) fr to directed True
    withEdgeIdsList eids (igraphDeleteEdges (_mgraph gr))
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False

-- | Set node attribute.
setNodeAttr :: (PrimMonad m, Serialize v, Ord v)
            => Int   -- ^ Node id
            -> v
            -> MGraph (PrimState m) d v e
            -> m ()
setNodeAttr nodeId x gr = do
    x' <- nodeLab gr nodeId
    unsafePrimToPrim $ withByteString (encode x) $
        igraphHaskellAttributeVASSet (_mgraph gr) vertexAttr nodeId
    modifyMutVar' (_mlabelToNode gr) $
        M.insertWith (++) x [nodeId] . M.adjust (delete nodeId) x'

-- | Set edge attribute.
setEdgeAttr :: (PrimMonad m, Serialize e)
            => Int   -- ^ Edge id
            -> e
            -> MGraph (PrimState m) d v e
            -> m ()
setEdgeAttr edgeId x gr = unsafePrimToPrim $
    withByteString (encode x) $ igraphHaskellAttributeEASSet (_mgraph gr) edgeAttr edgeId

{-
-- | Removes loop and/or multiple edges from the graph.
simplify :: Bool   -- ^ If true, multiple edges will be removed.
         -> Bool   -- ^ If true, loops (self edges) will be removed.
         ->
         -> Graph d v e -> Graph d v e
simplify delMul delLoop fun gr = do
-}
