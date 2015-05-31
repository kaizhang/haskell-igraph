import Foreign hiding (new)
import Control.Monad
import Data.Serialize
import qualified Data.ByteString.Internal as B
import IGraph
import IGraph.Mutable
import IGraph.Read
import IGraph.Clique
import IGraph.Community
import Foreign.Ptr
import IGraph.Structure
import IGraph.Internal.Constants

import System.Environment

main = do
    [fl] <- getArgs
    g <- readAdjMatrixWeighted fl :: IO (LGraph U B.ByteString Double)
    let ws = map (abs . flip edgeLabByEid g) [0 .. nEdges g - 1]
    print $ (map.map) (flip vertexLab g) $ maximalCliques (0,0) g
    print $ (map.map) (flip vertexLab g) $ communityLeadingEigenvector g (Just ws) 1000
    print $ closeness [1,2] g Nothing IgraphAll True

