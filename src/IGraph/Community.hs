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
import IGraph.Internal.Data
import IGraph.Internal.Community
import IGraph.Internal.Arpack

communityLeadingEigenvector :: LGraph d v e
                            -> (LGraph d v e -> Maybe [Double])  -- ^ extract weights
                            -> Int  -- ^ number of steps
                            -> [[Int]]
communityLeadingEigenvector g@(LGraph gr) fn step = unsafePerformIO $ do
    arparck <- igraphArpackNew
    vec <- igraphVectorNew 0
    withArpackOptPtr arparck $ \ap -> withVectorPtr vec $ \vptr -> case fn g of
        Just xs -> do
            ws <- listToVector xs
            withVectorPtr ws $ \wptr ->
                igraphCommunityLeadingEigenvector gr wptr nullPtr vptr step ap nullPtr
                                                  False nullPtr nullPtr nullPtr nullFunPtr nullPtr  

        _ -> igraphCommunityLeadingEigenvector gr nullPtr nullPtr vptr step ap nullPtr
                                                  False nullPtr nullPtr nullPtr nullFunPtr nullPtr  
    xs <- vectorPtrToList vec
    return $ map f $ groupBy ((==) `on` snd) $ sortBy (comparing snd) $ zip [0..] xs
  where
    f = fst . unzip
