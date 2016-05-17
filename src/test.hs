{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.ByteString.Char8 as B
import IGraph
import IGraph.Generators
import IGraph.Layout
import System.Environment
import Data.Default

main = do
    gr <- erdosRenyiGame GNM 100 50 U False
    coord <- getLayout gr def
