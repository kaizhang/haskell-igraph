{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IGraph.Algorithms.Isomorphism
    ( getSubisomorphisms
    , isomorphic
    , isoclassCreate
    , isoclass3
    , isoclass4
    ) where

import           System.IO.Unsafe               (unsafePerformIO)
import Data.Singletons (SingI, Sing, sing, fromSing)

import Foreign
import Foreign.C.Types

import           IGraph
import           IGraph.Internal.Initialization (igraphInit)
import           IGraph.Mutable
{#import IGraph.Internal #}

#include "haskell_igraph.h"

getSubisomorphisms :: Graph d v1 e1  -- ^ graph to be searched in
                   -> Graph d v2 e2   -- ^ smaller graph
                   -> [[Int]]
getSubisomorphisms g1 g2 = unsafePerformIO $ allocaVectorPtr $ \vpptr -> do
    igraphGetSubisomorphismsVf2 gptr1 gptr2 nullPtr nullPtr nullPtr nullPtr vpptr
        nullFunPtr nullFunPtr nullPtr
    (map.map) truncate <$> toLists vpptr
  where
    gptr1 = _graph g1
    gptr2 = _graph g2
{-# INLINE getSubisomorphisms #-}
{#fun igraph_get_subisomorphisms_vf2 as ^
    { `IGraph'
    , `IGraph'
    , id `Ptr ()'
    , id `Ptr ()'
    , id `Ptr ()'
    , id `Ptr ()'
    , castPtr `Ptr VectorPtr'
    , id `FunPtr (Ptr IGraph -> Ptr IGraph -> CInt -> CInt -> Ptr () -> IO CInt)'
    , id `FunPtr (Ptr IGraph -> Ptr IGraph -> CInt -> CInt -> Ptr () -> IO CInt)'
    , id `Ptr ()'
    } -> `CInt' void- #}

-- | Determine whether two graphs are isomorphic.
isomorphic :: Graph d v1 e1
           -> Graph d v2 e2
           -> Bool
isomorphic g1 g2 = unsafePerformIO $ alloca $ \ptr -> do
    _ <- igraphIsomorphic (_graph g1) (_graph g2) ptr
    x <- peek ptr
    return (x /= 0)
{#fun igraph_isomorphic as ^ { `IGraph', `IGraph', id `Ptr CInt' } -> `CInt' void- #}

-- | Creates a graph from the given isomorphism class.
-- This function is implemented only for graphs with three or four vertices.
isoclassCreate :: forall d. SingI d
               => Int   -- ^ The number of vertices to add to the graph.
               -> Int   -- ^ The isomorphism class
               -> Graph d () ()
isoclassCreate size idx = unsafePerformIO $ do
    gp <- igraphInit >> igraphIsoclassCreate size idx directed
    unsafeFreeze $ MGraph gp
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False
{#fun igraph_isoclass_create as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `Int', `Int', `Bool'
    } -> `CInt' void- #}

isoclass3 :: forall d. SingI d => [Graph d () ()]
isoclass3 = map (isoclassCreate 3) (if directed then [0..15] else [0..3])
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False

isoclass4 :: forall d. SingI d => [Graph d () ()]
isoclass4 = map (isoclassCreate 4) (if directed then [0..217] else [0..10])
  where
    directed = case fromSing (sing :: Sing d) of
        D -> True
        U -> False
