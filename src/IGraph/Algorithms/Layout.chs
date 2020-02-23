{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Algorithms.Layout
    ( layout
    , LayoutMethod(..)
    , kamadaKawai
    , lgl
    ) where

import           Data.Maybe             (isJust, fromMaybe)
import           Foreign                (nullPtr)
import           System.IO.Unsafe          (unsafePerformIO)

import Foreign

import           IGraph
import IGraph.Random
{#import IGraph.Internal #}

#include "haskell_igraph.h"

layout :: Graph d v e -> LayoutMethod -> Gen -> [(Double, Double)]
layout gr method _ = unsafePerformIO $ case method of
    Random -> allocaMatrix $ \mat -> do
        igraphLayoutRandom gptr mat
        getResult mat

    KamadaKawai seed niter kkconst epsilon -> do
        let f mat = igraphLayoutKamadaKawai gptr mat (isJust seed) niter
                epsilon (fromMaybe (fromIntegral $ nNodes gr) kkconst) nullPtr
                nullPtr nullPtr nullPtr nullPtr
        case seed of
            Nothing -> allocaMatrix $ \mat -> do
                f mat
                getResult mat
            Just s -> withRowLists ((\(x,y) -> [x,y]) $ unzip s) $ \mat -> do
                f mat
                getResult mat

    LGL niter delta area coolexp repulserad cellsize -> allocaMatrix $ \mat -> do
        igraphLayoutLgl gptr mat niter (delta n) (area n) coolexp
            (repulserad n) (cellsize n) (-1)
        getResult mat
  where
    n = nNodes gr
    gptr = _graph gr
    getResult mat = (\[x, y] -> zip x y) <$> toColumnLists mat

data LayoutMethod =
    Random 
  | KamadaKawai { kk_seed      :: Maybe [(Double, Double)]
                , kk_nIter     :: Int
                , kk_const     :: Maybe Double  -- ^ The Kamada-Kawai vertex attraction constant
                , kk_epsilon   :: Double
                }   -- ^ The Kamada-Kawai algorithm. Time complexity: O(|V|)
                    -- for each iteration, after an O(|V|^2 log|V|)
                    -- initialization step. 
  | LGL { lgl_nIter      :: !Int
        , lgl_maxdelta   :: (Int -> Double)  -- ^ The maximum length of the move allowed
        -- for a vertex in a single iteration. A reasonable default is the number of vertices.
        , lgl_area       :: (Int -> Double)  -- ^ This parameter gives the area
        -- of the square on which the vertices will be placed. A reasonable
        -- default value is the number of vertices squared.
        , lgl_coolexp    :: !Double  -- ^ The cooling exponent. A reasonable default value is 1.5.
        , lgl_repulserad :: (Int -> Double) -- ^ Determines the radius at which
        -- vertex-vertex repulsion cancels out attraction of adjacent vertices.
        -- A reasonable default value is area times the number of vertices.
        , lgl_cellsize   :: (Int -> Double)
        }

-- | Default parameters for the Kamada-Kawai algorithm.
kamadaKawai :: LayoutMethod
kamadaKawai = KamadaKawai
    { kk_seed = Nothing
    , kk_nIter = 10
    , kk_const = Nothing
    , kk_epsilon = 0 }

-- | Default parameters for the LGL algorithm.
lgl :: LayoutMethod
lgl = LGL
    { lgl_nIter = 100
    , lgl_maxdelta = \x -> fromIntegral x
    , lgl_area = area
    , lgl_coolexp = 1.5
    , lgl_repulserad = \x -> fromIntegral x * area x
    , lgl_cellsize = \x -> area x ** 0.25
    }
  where
    area x = fromIntegral $ x^2

-- | Places the vertices uniform randomly on a plane.
{#fun igraph_layout_random as ^
    { `IGraph'
    , castPtr `Ptr Matrix'
    } -> `CInt' void- #}

{#fun igraph_layout_kamada_kawai as ^
    { `IGraph'                    -- ^ Graph
    , castPtr `Ptr Matrix'        -- ^ Pointer to the result matrix
    , `Bool'                      -- ^ Whether to use the seed
    , `Int'                       -- ^ The maximum number of iterations to perform
    , `Double'                    -- ^ epsilon
    , `Double'                    -- ^ kkconst
    , castPtr `Ptr Vector'        -- ^ edges weights
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    } -> `CInt' void- #}

{# fun igraph_layout_lgl as ^
    { `IGraph'
    , castPtr `Ptr Matrix'
    , `Int'
    , `Double'
    , `Double'
    , `Double'
    , `Double'
    , `Double'
    , `Int'
    } -> `CInt' void- #}