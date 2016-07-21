{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module IGraph.Exporter.Graphics
    ( renderGraph
    , graphToDiagram
    ) where

import           Data.List              (sortBy)
import           Data.Ord               (comparing)
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude
import           Diagrams.Size          (dims)

import           IGraph
import           IGraph.Exporter.GEXF

renderGraph :: Graph d => FilePath -> Double -> Double -> LGraph d NodeAttr EdgeAttr -> IO ()
renderGraph out w h gr = renderCairo out (dims $ w ^& h) $ graphToDiagram gr

graphToDiagram :: Graph d => LGraph d NodeAttr EdgeAttr -> Diagram B
graphToDiagram gr = mconcat $ fst $ unzip $ sortBy (flip (comparing snd)) $
    map drawNode (nodes gr) ++ map drawEdge (edges gr)
  where
    drawNode x = ( moveTo (_positionX nattr ^& _positionY nattr)
                          (circle (_size nattr) # lwO 0 # fcA (_nodeColour nattr))
                 , _nodeZindex nattr )
      where
        nattr = nodeLab gr x
    drawEdge (from, to) = {-arrowBetween'
        ( with & arrowTail .~ noTail
               & arrowHead .~ arrowH
               & headLength .~ output (_edgeArrowLength eattr)
        ) start end-}
        ( fromVertices [start, end]
        # lwO (_edgeWeight eattr) # lcA (_edgeColour eattr), _edgeZindex eattr )
      where
        eattr = edgeLab gr (from, to)
        start = _positionX nattr1 ^& _positionY nattr1
        end = _positionX nattr2 ^& _positionY nattr2
        nattr1 = nodeLab gr from
        nattr2 = nodeLab gr to
    --arrowH | isDirected gr = dart
    --       | otherwise = noHead
{-# INLINE graphToDiagram #-}
