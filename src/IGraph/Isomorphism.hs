module IGraph.Isomorphism (getSubisomorphisms) where

import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe            (unsafePerformIO)

import           IGraph
import           IGraph.Internal.Data
import           IGraph.Internal.Isomorphism

getSubisomorphisms :: Graph d
                   => LGraph d v1 e1  -- ^ graph to be searched in
                   -> LGraph d v2 e2   -- ^ smaller graph
                   -> [[Int]]
getSubisomorphisms g1 g2 = unsafePerformIO $ do
    vpptr <- igraphVectorPtrNew 0
    igraphGetSubisomorphismsVf2 gptr1 gptr2 nullPtr nullPtr nullPtr nullPtr vpptr
        nullFunPtr nullFunPtr nullPtr
    (map.map) truncate <$> vectorPPtrToList vpptr
  where
    gptr1 = _graph g1
    gptr2 = _graph g2
{-# INLINE getSubisomorphisms #-}
