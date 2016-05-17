{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module IGraph.Exporter.Graphics
    ( renderGraph
    , graphToDiagram
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import IGraph
import IGraph.Exporter.GEXF

renderGraph :: FilePath -> Double -> Double -> LGraph d NodeAttr EdgeAttr -> IO ()
readerGraph out gr = renderSVG out (Dims w h) $ graphToDiagram gr

graphToDiagram :: Graph d => LGraph d NodeAttr EdgeAttr -> Diagram B
graphToDiagram gr = position (map drawNode (nodes gr)) <> mconcat (map drawEdge (edges gr))
  where
    drawNode x = ( _positionX nattr ^& _positionY nattr
                 , circle (_size nattr) # lwO 0 # fcA (_nodeColour nattr) )
      where
        nattr = nodeLab gr x
    drawEdge (from, to) =
        fromVertices [ _positionX nattr1 ^& _positionY nattr1
                     , _positionX nattr2 ^& _positionY nattr2 ]
      where
        eattr = edgeLab gr (from, to)
        nattr1 = nodeLab gr from
        nattr2 = nodeLab gr to
{-# INLINE graphToDiagram #-}
