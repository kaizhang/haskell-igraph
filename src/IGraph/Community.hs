module IGraph.Community
    ( communityLeadingEigenvector
    ) where

import Control.Monad
import Control.Applicative ((<$>))
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Data.List
import Data.Ord
import Data.Function (on)

import IGraph
import IGraph.Mutable (U)
import IGraph.Internal.Data
import IGraph.Internal.Community
import IGraph.Internal.Arpack

communityLeadingEigenvector :: LGraph U v e
                            -> Maybe [Double]  -- ^ extract weights
                            -> Int  -- ^ number of steps
                            -> [[Int]]
communityLeadingEigenvector gr ws step = unsafePerformIO $ do
    ap <- igraphArpackNew
    vptr <- igraphVectorNew 0
    wptr <- case ws of
        Just w -> listToVector w
        _ -> liftM VectorPtr $ newForeignPtr_ $ castPtr nullPtr
    igraphCommunityLeadingEigenvector (_graph gr) wptr nullPtr vptr step ap nullPtr
                                      False nullPtr nullPtr nullPtr nullFunPtr nullPtr  
    xs <- vectorPtrToList vptr
    return $ map f $ groupBy ((==) `on` snd) $ sortBy (comparing snd) $ zip [0..] xs
  where
    f = fst . unzip
