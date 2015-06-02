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
    g <- readAdjMatrix fl :: IO (LGraph U B.ByteString ())
    let n = nNodes g
        r = map (f g) [0..n-1]
    mapM_ h r
  where
    f g i = let name = nodeLab g i
                xs = map (nodeLab g) $ neighbors g i
            in (name, B.intercalate "," xs)
    h (a,b) = do
        B.putStr a
        B.putStr "\t"
        B.putStrLn b
