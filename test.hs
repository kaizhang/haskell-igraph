{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Foreign hiding (new)
import Control.Monad
import Data.Serialize
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import IGraph
import IGraph.Mutable
import IGraph.Read
import IGraph.Clique
import IGraph.Community
import Foreign.Ptr
import IGraph.Structure
import IGraph.Internal.Constants

import System.Environment

main = do
    [fl] <- getArgs
    g <- readAdjMatrixWeighted fl :: IO (LGraph U B.ByteString Double)
    let m = _labelToNode g
        [a] = M.lookupDefault [] "H3K27ac" m
        [b] = M.lookupDefault [] "H3K4me1" m
        ws = Just $ map (abs . edgeLabByEid g) [0 .. nEdges g - 1]
        cs = (map.map) (nodeLab g) $ communityLeadingEigenvector g ws 10000
    print $ edgeLab g (b,a)
    mapM_ print cs
