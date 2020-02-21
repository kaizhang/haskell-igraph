module IGraph.Random
    ( Gen
    , withSeed
    ) where

import IGraph.Internal

-- | Random number generator
data Gen = Gen

{-
withSystemRandom :: (Gen -> IO a) -> IO a
withSystemRandom fun = fun Gen
{-# INLINE withSystemRandom #-}
-}

withSeed :: Int -> (Gen -> IO a) -> IO a
withSeed seed fun = do
    rng <- igraphRngDefault
    igraphRngSeed rng seed
    fun Gen
{-# INLINE withSeed #-}