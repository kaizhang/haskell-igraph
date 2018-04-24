{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Isomorphism
    ( getSubisomorphisms
    , isomorphic
    , isoclassCreate
    , isoclass3
    , isoclass4
    ) where

import           System.IO.Unsafe               (unsafePerformIO)

import Foreign
import Foreign.C.Types

import           IGraph
import           IGraph.Internal.Initialization (igraphInit)
import           IGraph.Mutable
{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "haskell_igraph.h"

getSubisomorphisms :: Graph d
                   => LGraph d v1 e1  -- ^ graph to be searched in
                   -> LGraph d v2 e2   -- ^ smaller graph
                   -> [[Int]]
getSubisomorphisms g1 g2 = unsafePerformIO $ do
    vpptr <- igraphVectorPtrNew 0
    igraphGetSubisomorphismsVf2 gptr1 gptr2 nullPtr nullPtr nullPtr nullPtr vpptr
        nullFunPtr nullFunPtr nullPtr
    (map.map) truncate <$> toLists vpptr
  where
    gptr1 = _graph g1
    gptr2 = _graph g2
{-# INLINE getSubisomorphisms #-}
{#fun igraph_get_subisomorphisms_vf2 as ^ { `IGraph', `IGraph',
    id `Ptr ()', id `Ptr ()', id `Ptr ()', id `Ptr ()', `VectorPtr',
    id `FunPtr (Ptr IGraph -> Ptr IGraph -> CInt -> CInt -> Ptr () -> IO CInt)',
    id `FunPtr (Ptr IGraph -> Ptr IGraph -> CInt -> CInt -> Ptr () -> IO CInt)',
    id `Ptr ()'} -> `Int' #}

-- | Determine whether two graphs are isomorphic.
isomorphic :: Graph d
           => LGraph d v1 e1
           -> LGraph d v2 e2
           -> Bool
isomorphic g1 g2 = unsafePerformIO $ alloca $ \ptr -> do
    _ <- igraphIsomorphic (_graph g1) (_graph g2) ptr
    x <- peek ptr
    return (x /= 0)
{#fun igraph_isomorphic as ^ { `IGraph', `IGraph', id `Ptr CInt' } -> `Int' #}

-- | Creates a graph from the given isomorphism class.
-- This function is implemented only for graphs with three or four vertices.
isoclassCreate :: Graph d
               => Int   -- ^ The number of vertices to add to the graph.
               -> Int   -- ^ The isomorphism class
               -> d
               -> LGraph d () ()
isoclassCreate size idx d = unsafePerformIO $ do
    gp <- igraphInit >> igraphIsoclassCreate size idx (isD d)
    unsafeFreeze $ MLGraph gp

isoclass3 :: Graph d => d -> [LGraph d () ()]
isoclass3 d = map (flip (isoclassCreate 3) d) n
  where
    n | isD d = [0..15]
      | otherwise = [0..3]

isoclass4 :: Graph d => d -> [LGraph d () ()]
isoclass4 d = map (flip (isoclassCreate 4) d) n
  where
    n | isD d = [0..217]
      | otherwise = [0..10]
