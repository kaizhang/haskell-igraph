module Test.Isomorphism
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
import IGraph.Isomorphism

tests :: TestTree
tests = testGroup "Isomorphism"
    [ graphIsomorphism ]

graphIsomorphism :: TestTree
graphIsomorphism = testCase "Graph isomorphism" $ assertBool "" $
    and (zipWith isomorphic triad triad) &&
    (not . or) (zipWith isomorphic triad $ reverse triad)
