module IGraph.Community
    ( CommunityOpt(..)
    , CommunityMethod(..)
    , findCommunity
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad
import           Data.Default.Class
import           Data.Function             (on)
import           Data.List
import           Data.Ord
import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe          (unsafePerformIO)

import           IGraph
import           IGraph.Internal.Arpack
import           IGraph.Internal.Community
import           IGraph.Internal.Constants
import           IGraph.Internal.Data

data CommunityOpt = CommunityOpt
    { _method    :: CommunityMethod
    , _weights   :: Maybe [Double]
    , _nIter     :: Int  -- ^ [LeadingEigenvector] number of iterations, default is 10000
    , _nSpins    :: Int  -- ^ [Spinglass] number of spins, default is 25
    , _startTemp :: Double  -- ^ [Spinglass] the temperature at the start
    , _stopTemp  :: Double  -- ^ [Spinglass] the algorithm stops at this temperature
    , _coolFact  :: Double  -- ^ [Spinglass] the cooling factor for the simulated annealing
    , _gamma     :: Double  -- ^ [Spinglass] the gamma parameter of the algorithm.
    }

data CommunityMethod = LeadingEigenvector
                     | Spinglass

instance Default CommunityOpt where
    def = CommunityOpt
        { _method = LeadingEigenvector
        , _weights = Nothing
        , _nIter = 10000
        , _nSpins = 25
        , _startTemp = 1.0
        , _stopTemp = 0.01
        , _coolFact = 0.99
        , _gamma = 1.0
        }

findCommunity :: LGraph U v e -> CommunityOpt -> [[Int]]
findCommunity gr opt = unsafePerformIO $ do
    result <- igraphVectorNew 0
    ws <- case _weights opt of
        Just w -> fromList w
        _      -> liftM Vector $ newForeignPtr_ $ castPtr nullPtr

    case _method opt of
        LeadingEigenvector -> do
            ap <- igraphArpackNew
            igraphCommunityLeadingEigenvector (_graph gr) ws nullPtr result
                                              (_nIter opt) ap nullPtr False
                                              nullPtr nullPtr nullPtr
                                              nullFunPtr nullPtr
        Spinglass ->
            igraphCommunitySpinglass (_graph gr) ws nullPtr nullPtr result
                                     nullPtr (_nSpins opt) False (_startTemp opt)
                                     (_stopTemp opt) (_coolFact opt)
                                     IgraphSpincommUpdateConfig (_gamma opt)
                                     IgraphSpincommImpOrig 1.0

    liftM ( map (fst . unzip) . groupBy ((==) `on` snd)
          . sortBy (comparing snd) . zip [0..] ) $ toList result
