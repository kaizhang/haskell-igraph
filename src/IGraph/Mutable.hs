{-# LANGUAGE MultiParamTypeClasses #-}
module IGraph.Mutable
    ( MGraph(..)
    , MLGraph(..)
    , setEdgeAttr
    , setNodeAttr
    , edgeAttr
    , vertexAttr
    )where

import           Control.Monad                  (when)
import           Control.Monad.Primitive
import qualified Data.ByteString.Char8          as B
import           Data.Serialize                 (Serialize)
import           Foreign
import           Foreign.C.String               (CString, withCString)

import           IGraph.Internal.Attribute
import           IGraph.Internal.Data
import           IGraph.Internal.Graph
import           IGraph.Internal.Initialization
import           IGraph.Internal.Selector
import           IGraph.Types

vertexAttr :: String
vertexAttr = "vertex_attribute"

edgeAttr :: String
edgeAttr = "edge_attribute"

withVertexAttr :: (CString -> IO a) -> IO a
withVertexAttr = withCString vertexAttr
{-# INLINE withVertexAttr #-}

withEdgeAttr :: (CString -> IO a) -> IO a
withEdgeAttr = withCString edgeAttr
{-# INLINE withEdgeAttr #-}

class MGraph d where
    new :: PrimMonad m => Int -> m (MLGraph (PrimState m) d v e)

    addNodes :: PrimMonad m => Int -> MLGraph(PrimState m) d v e -> m ()
    addNodes n (MLGraph g) = unsafePrimToPrim $ igraphAddVertices g n nullPtr

    addLNodes :: (Serialize v, PrimMonad m)
              => Int  -- ^ the number of new vertices add to the graph
              -> [v]  -- ^ vertices' labels
              -> MLGraph (PrimState m) d v e -> m ()
    addLNodes n labels (MLGraph g)
        | n /= length labels = error "addLVertices: incorrect number of labels"
        | otherwise = unsafePrimToPrim $ withVertexAttr $ \vattr ->
            asBSVector labels $ \bsvec -> with (mkStrRec vattr bsvec) $ \ptr -> do
                vptr <- listToVectorP [castPtr ptr]
                withVectorPPtr vptr (igraphAddVertices g n . castPtr)

    delNodes :: PrimMonad m => [Int] -> MLGraph (PrimState m) d v e -> m ()
    delNodes ns (MLGraph g) = unsafePrimToPrim $ do
        vptr <- listToVector $ map fromIntegral ns
        vsptr <- igraphVsVector vptr
        igraphDeleteVertices g vsptr
        return ()

    addEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()
    addEdges es (MLGraph g) = unsafePrimToPrim $ do
        vec <- listToVector xs
        igraphAddEdges g vec nullPtr
      where
        xs = concatMap ( \(a,b) -> [fromIntegral a, fromIntegral b] ) es

    addLEdges :: (PrimMonad m, Serialize e) => [LEdge e] -> MLGraph (PrimState m) d v e -> m ()
    addLEdges es (MLGraph g) = unsafePrimToPrim $ withEdgeAttr $ \eattr ->
        asBSVector vs $ \bsvec -> with (mkStrRec eattr bsvec) $ \ptr -> do
            vec <- listToVector $ concat xs
            vptr <- listToVectorP [castPtr ptr]
            withVectorPPtr vptr (igraphAddEdges g vec . castPtr)
      where
        (xs, vs) = unzip $ map ( \(a,b,v) -> ([fromIntegral a, fromIntegral b], v) ) es

    delEdges :: PrimMonad m => [(Int, Int)] -> MLGraph (PrimState m) d v e -> m ()

instance MGraph U where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n False >>= return . MLGraph

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        vptr <- listToVector $ map fromIntegral eids
        esptr <- igraphEsVector vptr
        igraphDeleteEdges g esptr
        return ()
      where
        eids = flip map es $ \(fr, to) -> igraphGetEid g fr to False True

instance MGraph D where
    new n = unsafePrimToPrim $ igraphInit >>= igraphNew n True >>= return . MLGraph

    delEdges es (MLGraph g) = unsafePrimToPrim $ do
        vptr <- listToVector $ map fromIntegral eids
        esptr <- igraphEsVector vptr
        igraphDeleteEdges g esptr
        return ()
      where
        eids = flip map es $ \(fr, to) -> igraphGetEid g fr to True True

setNodeAttr :: (PrimMonad m, Serialize v)
            => Int   -- ^ Node id
            -> v
            -> MLGraph (PrimState m) d v e
            -> m ()
setNodeAttr nodeId x (MLGraph gr) = unsafePrimToPrim $ asBS x $ \bs ->
    with bs $ \bsptr -> do
        err <- igraphHaskellAttributeVASSet gr vertexAttr nodeId bsptr
        when (err /= 0) $ error "Fail to set node attribute!"

setEdgeAttr :: (PrimMonad m, Serialize v)
            => Int   -- ^ Edge id
            -> v
            -> MLGraph (PrimState m) d v e
            -> m ()
setEdgeAttr edgeId x (MLGraph gr) = unsafePrimToPrim $ asBS x $ \bs ->
    with bs $ \bsptr -> do
        err <- igraphHaskellAttributeEASSet gr edgeAttr edgeId bsptr
        when (err /= 0) $ error "Fail to set edge attribute!"
