{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module IGraph where

import Control.Monad.ST (runST)
import Control.Monad.Primitive
import Data.Maybe

import IGraph.Mutable
import IGraph.Internal.Graph
import IGraph.Internal.Attribute

type family Mutable (gr :: * -> * -> * -> *) :: * -> * -> * -> * -> *
type instance Mutable LGraph = MLGraph

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph :: IGraphPtr }

class MGraph (Mutable gr) d => Graph gr d where
    mkGraph :: (Show v, Show e) => (Int, Maybe [v]) -> ([(Int, Int)], Maybe [e]) -> gr d v e
    mkGraph (n, vattr) (es,eattr) = runST $ do
        g <- new 0
        let addV | isNothing vattr = addVertices n g
                 | otherwise = addLVertices n (fromJust vattr) g
            addE | isNothing eattr = addEdges es g
                 | otherwise = addLEdges (zip' es (fromJust eattr)) g
        addV
        addE
        unsafeFreeze g
      where
        zip' a b | length a /= length b = error "incorrect length"
                 | otherwise = zipWith (\(x,y) z -> (x,y,z)) a b

    vertexLab :: Read v => Int -> gr d v e -> v

    edgeLab :: Read e => (Int, Int) -> gr d v e -> e

    unsafeFreeze :: PrimMonad m => Mutable gr (PrimState m) d v e -> m (gr d v e)

    unsafeThaw :: PrimMonad m => gr d v e -> m (Mutable gr (PrimState m) d v e)


instance Graph LGraph U where
    vertexLab i (LGraph g) = read $ igraphCattributeVAS g vertexAttr i

    edgeLab (fr,to) (LGraph g) = read $ igraphCattributeEAS g edgeAttr $ igraphGetEid g fr to True True

    unsafeFreeze (MLGraph g) = return $ LGraph g

    unsafeThaw (LGraph g) = return $ MLGraph g
