module IGraph.Layout
    ( kamadaKawai
    ) where

import Foreign (nullPtr)
import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)

import IGraph
import IGraph.Internal.Clique
import IGraph.Internal.Layout
import IGraph.Internal.Data

data LayoutOpt = LayoutOpt
    { _seed :: Maybe [(Double, Double)]
    , _nIter :: Int
    } deriving (Show)

kamadaKawai :: Graph d => LGraph d v e -> Double -> Double -> Double -> Double -> LayoutOpt -> [(Double, Double)]
kamadaKawai gr sigma initemp coolexp kkconst opt = unsafePerformIO $ do
    mptr <- mat
    igraphLayoutKamadaKawai (_graph gr) mptr (_nIter opt) sigma initemp coolexp kkconst useSeed nullPtr nullPtr nullPtr nullPtr
    [x, y] <- matrixPtrToLists mptr
    return $ zip x y
  where
    (useSeed, mat) = case _seed opt of
        Just xs -> if length xs /= nNodes gr
                      then error "Seed error: incorrect size"
                      else (True, f xs)
        _ -> (False, igraphMatrixNew 0 0)
    f xs = let (x,y) = unzip xs
           in listsToMatrixPtr [x,y]
