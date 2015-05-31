module IGraph.Structure
    ( closeness
    ) where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

import IGraph
import IGraph.Mutable (U)
import IGraph.Internal.Data
import IGraph.Internal.Selector
import IGraph.Internal.Structure
import IGraph.Internal.Arpack
import IGraph.Internal.Constants

closeness :: [Int]  -- ^ vertices
          -> LGraph d v e
          -> Maybe [Double]  -- ^ optional edge weights
          -> IGraphNeimode
          -> Bool   -- ^ whether to normalize
          -> [Double]
closeness vs (LGraph g) ws mode normal = unsafePerformIO $ do
    vsptr <- igraphVsNew
    vs' <- listToVector $ map fromIntegral vs
    igraphVsVector vsptr vs'
    vptr <- igraphVectorNew 0
    ws' <- case ws of
        Just w -> listToVector w
        _ -> liftM VectorPtr $ newForeignPtr_ $ castPtr nullPtr
    igraphCloseness g vptr vsptr mode ws' normal
    vectorPtrToList vptr
