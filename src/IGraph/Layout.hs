module IGraph.Layout
    ( getLayout
    , LayoutOpt(..)
    , LayoutMethod(..)
    ) where

import Foreign (nullPtr)
import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)
import Data.Default.Class

import IGraph
import IGraph.Internal.Clique
import IGraph.Internal.Layout
import IGraph.Internal.Data

data LayoutOpt = LayoutOpt
    { _seed :: Maybe [(Double, Double)]
    , _nIter :: Int
    , _method :: LayoutMethod
    , _sigma :: (Int -> Double)  -- ^ [KamadaKawai] the base standard deviation of position change proposals
    , _startTemp :: Double  -- ^ [KamadaKawai] the initial temperature for the annealing
    , _coolFact :: Double  -- ^ [KamadaKawai] the cooling factor for the simulated annealing
    , _kkConst :: (Int -> Double)  -- ^ [KamadaKawai] The Kamada-Kawai vertex attraction constant
    }

instance Default LayoutOpt where
    def = LayoutOpt
        { _seed = Nothing
        , _nIter = 10000
        , _method = KamadaKawai
        , _sigma = \x -> fromIntegral x / 4
        , _startTemp = 10
        , _coolFact = 0.99
        , _kkConst = \x -> fromIntegral $ x^2
        }

data LayoutMethod = KamadaKawai

getLayout :: Graph d => LGraph d v e -> LayoutOpt -> IO [(Double, Double)]
getLayout gr opt = do
    mptr <- mat

    case _method opt of
        KamadaKawai -> igraphLayoutKamadaKawai gptr mptr iters s initemp coolexp
                       kkconst useSeed nullPtr nullPtr nullPtr nullPtr

    [x, y] <- matrixPtrToColumnLists mptr
    return $ zip x y
  where
    n = nNodes gr
    gptr = _graph gr
    iters = _nIter opt
    s = _sigma opt n
    initemp = _startTemp opt
    coolexp = _coolFact opt
    kkconst = _kkConst opt n
    (useSeed, mat) = case _seed opt of
        Just xs -> if length xs /= nNodes gr
                      then error "Seed error: incorrect size"
                      else (True, f xs)
        _ -> (False, igraphMatrixNew 0 0)
    f xs = let (x,y) = unzip xs
           in listsToMatrixPtr [x,y]
