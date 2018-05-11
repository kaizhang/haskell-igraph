{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module IGraph.Algorithms.Community
    ( modularity
    , findCommunity
    , CommunityMethod(..)
    , defaultLeadingEigenvector
    , defaultSpinglass
    ) where

import           Data.Function             (on)
import           Data.List (sortBy, groupBy)
import Data.List.Ordered (nubSortBy)
import           Data.Ord (comparing)
import           System.IO.Unsafe          (unsafePerformIO)

import           Foreign
import           Foreign.C.Types

import           IGraph
import IGraph.Internal.C2HS
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}

#include "haskell_igraph.h"

modularity :: Graph d v e
           -> [[Int]]   -- ^ Communities.
           -> Maybe [Double] -- ^ Weights
           -> Double
modularity gr clusters ws
    | length nds /= length (concat clusters) = error "Duplicated nodes"
    | nds /= nodes gr = error "Some nodes were not given community assignments"
    | otherwise = unsafePerformIO $ withList membership $ \membership' ->
        withListMaybe ws (igraphModularity (_graph gr) membership')
  where
    (membership, nds) = unzip $ nubSortBy (comparing snd) $ concat $
        zipWith f [0 :: Int ..] clusters
      where
        f i xs = zip (repeat i) xs
{#fun igraph_modularity as ^
    { `IGraph'
    , castPtr `Ptr Vector'
	, alloca- `Double' peekFloatConv*
	, castPtr `Ptr Vector'
    } -> `CInt' void- #}

data CommunityMethod =
      LeadingEigenvector
        { _nIter     :: Int  -- ^ number of iterations, default is 10000
        }
    | Spinglass
        { _nSpins    :: Int  -- ^ number of spins, default is 25
        , _startTemp :: Double  -- ^ the temperature at the start
        , _stopTemp  :: Double  -- ^ the algorithm stops at this temperature
        , _coolFact  :: Double  -- ^ the cooling factor for the simulated annealing
        , _gamma     :: Double  -- ^ the gamma parameter of the algorithm.
        }

defaultLeadingEigenvector :: CommunityMethod
defaultLeadingEigenvector = LeadingEigenvector 10000

defaultSpinglass :: CommunityMethod
defaultSpinglass = Spinglass
    { _nSpins = 25
    , _startTemp = 1.0
    , _stopTemp = 0.01
    , _coolFact = 0.99
    , _gamma = 1.0 }

findCommunity :: Graph 'U v e
              -> Maybe [Double]   -- ^ node weights
              -> CommunityMethod  -- ^ Community finding algorithms
              -> [[Int]]
findCommunity gr ws method = unsafePerformIO $ allocaVector $ \result ->
    withListMaybe ws $ \ws' -> do
        case method of
            LeadingEigenvector n -> allocaArpackOpt $ \arpack ->
                igraphCommunityLeadingEigenvector (_graph gr) ws' nullPtr result
                                                  n arpack nullPtr False
                                                  nullPtr nullPtr nullPtr
                                                  nullFunPtr nullPtr
            Spinglass{..} -> igraphCommunitySpinglass (_graph gr) ws' nullPtr nullPtr result
                                     nullPtr _nSpins False _startTemp
                                     _stopTemp _coolFact
                                     IgraphSpincommUpdateConfig _gamma
                                     IgraphSpincommImpOrig 1.0

        fmap ( map (fst . unzip) . groupBy ((==) `on` snd)
              . sortBy (comparing snd) . zip [0..] ) $ toList result

{#fun igraph_community_spinglass as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , id `Ptr CDouble'
    , id `Ptr CDouble'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , `Int'
    , `Bool'
    , `Double'
    , `Double'
    , `Double'
    , `SpincommUpdate'
    , `Double'
    , `SpinglassImplementation'
    , `Double'
    } -> `CInt' void- #}

{#fun igraph_community_leading_eigenvector as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Matrix'
    , castPtr `Ptr Vector'
    , `Int'
    , castPtr `Ptr ArpackOpt'
    , id `Ptr CDouble'
    , `Bool'
    , castPtr `Ptr Vector'
    , castPtr `Ptr VectorPtr'
    , castPtr `Ptr Vector'
    , id `T'
    , id `Ptr ()'
    } -> `CInt' void- #}

type T = FunPtr ( Ptr ()
                -> CLong
                -> CDouble
                -> Ptr ()
                -> FunPtr (Ptr CDouble -> Ptr CDouble -> CInt -> Ptr () -> IO CInt)
                -> Ptr ()
                -> Ptr ()
                -> IO CInt)
