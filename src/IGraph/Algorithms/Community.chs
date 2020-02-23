{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module IGraph.Algorithms.Community
    ( findCommunity
    , CommunityMethod(..)
    , leadingEigenvector
    , spinglass
    , leiden
    , modularity
    ) where

import           Data.Function             (on)
import           Data.List (sortBy, groupBy)
import Data.List.Ordered (nubSortBy)
import           Data.Ord (comparing)
import           System.IO.Unsafe          (unsafePerformIO)
import           Data.Serialize            (Serialize)

import           Foreign
import           Foreign.C.Types

import           IGraph
import           IGraph.Random
import IGraph.Internal.C2HS
{#import IGraph.Internal #}
{#import IGraph.Internal.Constants #}

#include "haskell_igraph.h"

-- | Detecting community structure.
findCommunity :: (Serialize v, Serialize e)
              => Graph 'U v e
              -> Maybe (v -> Double)  -- ^ Function to assign node weights
              -> Maybe (e -> Double)  -- ^ Function to assign edge weights
              -> CommunityMethod  -- ^ Community finding algorithms
              -> Gen
              -> [[Int]]
findCommunity gr getNodeW getEdgeW method _ = unsafePerformIO $ allocaVector $ \result ->
    withListMaybe ew $ \ew' -> do
        case method of
            LeadingEigenvector n -> allocaArpackOpt $ \arpack ->
                igraphCommunityLeadingEigenvector (_graph gr) ew' nullPtr result
                                                  n arpack nullPtr False
                                                  nullPtr nullPtr nullPtr
                                                  nullFunPtr nullPtr
            Spinglass{..} -> igraphCommunitySpinglass (_graph gr) ew' nullPtr nullPtr result
                                     nullPtr _nSpins False _startTemp
                                     _stopTemp _coolFact
                                     IgraphSpincommUpdateConfig _gamma
                                     IgraphSpincommImpOrig 1.0
            Leiden{..} -> do
                _ <- withListMaybe nw $ \nw' -> igraphCommunityLeiden
                    (_graph gr) ew' nw' _resolution _beta False result nullPtr
                return ()
        fmap ( map (fst . unzip) . groupBy ((==) `on` snd)
              . sortBy (comparing snd) . zip [0..] ) $ toList result
  where
    ew = case getEdgeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labEdges gr
    nw = case getNodeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labNodes gr

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
    | Leiden
        { _resolution :: Double
        , _beta :: Double
        }

-- | Default parameters for the leading eigenvector algorithm.
leadingEigenvector :: CommunityMethod
leadingEigenvector = LeadingEigenvector 10000

-- | Default parameters for the spin-glass algorithm.
spinglass :: CommunityMethod
spinglass = Spinglass
    { _nSpins = 25
    , _startTemp = 1.0
    , _stopTemp = 0.01
    , _coolFact = 0.99
    , _gamma = 1.0 }

-- | Default parameters for the leiden algorithm.
leiden :: CommunityMethod
leiden = Leiden
    { _resolution = 1
    , _beta = 0 }

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

{#fun igraph_community_leiden as ^
    { `IGraph'
    , castPtr `Ptr Vector'
    , castPtr `Ptr Vector'
    , `Double'
    , `Double'
    , `Bool'
    , castPtr `Ptr Vector'
    , alloca- `Int' peekIntConv*
    , id `Ptr CDouble'
    } -> `CInt' void- #}

type T = FunPtr ( Ptr ()
                -> CLong
                -> CDouble
                -> Ptr ()
                -> FunPtr (Ptr CDouble -> Ptr CDouble -> CInt -> Ptr () -> IO CInt)
                -> Ptr ()
                -> Ptr ()
                -> IO CInt)

-- | Calculate the modularity of a graph with respect to some vertex types.
modularity :: Serialize e
           => Graph d v e
           -> Maybe (e -> Double)  -- ^ Function to assign edge weights
           -> [[Int]]   -- ^ Communities.
           -> Double
modularity gr getEdgeW clusters
    | length nds /= length (concat clusters) = error "Duplicated nodes"
    | nds /= nodes gr = error "Some nodes were not given community assignments"
    | otherwise = unsafePerformIO $ withList membership $ \membership' ->
        withListMaybe ws (igraphModularity (_graph gr) membership')
  where
    (membership, nds) = unzip $ nubSortBy (comparing snd) $ concat $
        zipWith f [0 :: Int ..] clusters
      where
        f i xs = zip (repeat i) xs
    ws = case getEdgeW of
        Nothing -> Nothing
        Just f -> Just $ map (f . snd) $ labEdges gr
{#fun igraph_modularity as ^
    { `IGraph'
    , castPtr `Ptr Vector'
	, alloca- `Double' peekFloatConv*
	, castPtr `Ptr Vector'
    } -> `CInt' void- #}

