{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Layout
    ( getLayout
    , LayoutMethod(..)
    , defaultKamadaKawai
    , defaultLGL
    ) where

import           Data.Maybe             (isJust)
import           Foreign                (nullPtr)

import qualified Foreign.Ptr as C2HSImp
import Foreign

import           IGraph
{#import IGraph.Internal #}

#include "igraph/igraph.h"

data LayoutMethod =
    KamadaKawai { kk_seed      :: !(Maybe [(Double, Double)])
                , kk_nIter     :: !Int
                , kk_sigma     :: (Int -> Double) -- ^ The base standard deviation of
                -- position change proposals
                , kk_startTemp :: !Double  -- ^ The initial temperature for the annealing
                , kk_coolFact  :: !Double  -- ^ The cooling factor for the simulated annealing
                , kk_const     :: (Int -> Double)  -- ^ The Kamada-Kawai vertex attraction constant
                }
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

defaultKamadaKawai :: LayoutMethod
defaultKamadaKawai = KamadaKawai
    { kk_seed = Nothing
    , kk_nIter = 10
    , kk_sigma = \x -> fromIntegral x / 4
    , kk_startTemp = 10
    , kk_coolFact = 0.99
    , kk_const = \x -> fromIntegral $ x^2
    }

defaultLGL :: LayoutMethod
defaultLGL = LGL
    { lgl_nIter = 100
    , lgl_maxdelta = \x -> fromIntegral x
    , lgl_area = area
    , lgl_coolexp = 1.5
    , lgl_repulserad = \x -> fromIntegral x * area x
    , lgl_cellsize = \x -> area x ** 0.25
    }
  where
    area x = fromIntegral $ x^2

getLayout :: Graph d v e -> LayoutMethod -> IO [(Double, Double)]
getLayout gr method = case method of
    KamadaKawai seed niter sigma initemp coolexp kkconst -> case seed of
        Nothing -> allocaMatrix $ \mat -> do
            igraphLayoutKamadaKawai gptr mat niter (sigma n) initemp coolexp
                (kkconst n) (isJust seed) nullPtr nullPtr nullPtr nullPtr
            [x, y] <- toColumnLists mat
            return $ zip x y
        Just xs -> if length xs /= nNodes gr
            then error "Seed error: incorrect size"
            else withRowLists ((\(x,y) -> [x,y]) (unzip xs)) $ \mat -> do
                igraphLayoutKamadaKawai gptr mat niter (sigma n) initemp coolexp
                    (kkconst n) (isJust seed) nullPtr nullPtr nullPtr nullPtr
                [x, y] <- toColumnLists mat
                return $ zip x y

    LGL niter delta area coolexp repulserad cellsize -> allocaMatrix $ \mat -> do
        igraphLayoutLgl gptr mat niter (delta n) (area n) coolexp
            (repulserad n) (cellsize n) (-1)
        [x, y] <- toColumnLists mat
        return $ zip x y
  where
    n = nNodes gr
    gptr = _graph gr

{#fun igraph_layout_kamada_kawai as ^
    { `IGraph'
    , castPtr `Ptr Matrix'
    , `Int'
    , `Double'
    , `Double'
    , `Double'
    , `Double'
    , `Bool'
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
