module Test.Motif
    ( tests
    ) where

import           Control.Arrow
import           Control.Monad.ST
import           Data.List
import qualified Data.Matrix.Unboxed as M
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           IGraph
import           IGraph
import           IGraph.Motif

tests :: TestTree
tests = testGroup "Network motif"
    [ testCase "triad Census" $ M.toLists (M.ident 16 :: M.Matrix Int) @=?
        map triadCensus triad ]
