{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module IGraph where

import Control.Monad.ST (runST)
import Control.Monad.Primitive
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

import IGraph.Mutable
import IGraph.Internal.Graph
import IGraph.Internal.Constants
import IGraph.Internal.Attribute
import IGraph.Internal.Selector

type family Mutable (gr :: * -> * -> * -> *) :: * -> * -> * -> * -> *
type instance Mutable LGraph = MLGraph

-- | graph with labeled nodes and edges
data LGraph d v e = LGraph
    { _graph :: IGraphPtr
    , _nodeLabelToId :: M.HashMap v [Int] }

class MGraph (Mutable gr) d => Graph gr d where
    nNodes :: gr d v e -> Int
    nEdges :: gr d v e -> Int

    mkGraph :: (Hashable v, Read v, Eq v, Show v, Show e) => (Int, Maybe [v]) -> ([(Int, Int)], Maybe [e]) -> gr d v e
    mkGraph (n, vattr) (es,eattr) = runST $ do
        g <- new 0
        let addV | isNothing vattr = addNodes n g
                 | otherwise = addLNodes n (fromJust vattr) g
            addE | isNothing eattr = addEdges es g
                 | otherwise = addLEdges (zip' es (fromJust eattr)) g
        addV
        addE
        unsafeFreeze g
      where
        zip' a b | length a /= length b = error "incorrect length"
                 | otherwise = zipWith (\(x,y) z -> (x,y,z)) a b

    nodeLab :: Read v => gr d v e -> Int -> v

    edgeLab :: Read e => gr d v e -> (Int, Int) -> e

    edgeLabByEid :: Read e => gr d v e -> Int -> e

    unsafeFreeze :: (Hashable v, Eq v, Read v, PrimMonad m) => Mutable gr (PrimState m) d v e -> m (gr d v e)

    unsafeThaw :: PrimMonad m => gr d v e -> m (Mutable gr (PrimState m) d v e)


instance Graph LGraph U where
    nNodes (LGraph g _) = igraphVcount g

    nEdges (LGraph g _) = igraphEcount g

    nodeLab (LGraph g _) i = read $ igraphCattributeVAS g vertexAttr i

    edgeLab (LGraph g _) (fr,to) = read $ igraphCattributeEAS g edgeAttr $ igraphGetEid g fr to True True

    edgeLabByEid (LGraph g _) i = read $ igraphCattributeEAS g edgeAttr i

    unsafeFreeze (MLGraph g) = return $ LGraph g labToId
      where
        labToId = M.fromListWith (++) $ zip labels $ map return [0..nV-1]
        nV = igraphVcount g
        labels = map (read . igraphCattributeVAS g vertexAttr) [0 .. nV-1]

    unsafeThaw (LGraph g _) = return $ MLGraph g


neighbors :: LGraph d v e -> Int -> [Int]
neighbors gr i = unsafePerformIO $ do
    vs <- igraphVsNew
    igraphVsAdj vs i IgraphAll
    vit <- igraphVitNew (_graph gr) vs
    loop vit
  where
    loop x = do
        isEnd <- igraphVitEnd x
        if isEnd
           then return []
           else do
               cur <- igraphVitGet x
               igraphVitNext x
               acc <- loop x
               return $ cur : acc

