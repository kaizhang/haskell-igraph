import Foreign hiding (new)
import Control.Monad
import Data.Serialize
import qualified Data.ByteString.Internal as B
import IGraph
import IGraph.Mutable
import IGraph.Read
import IGraph.Clique
import IGraph.Community
import IGraph.Internal.Graph
import IGraph.Internal.Generator
import IGraph.Internal.Attribute
import IGraph.Internal.Initialization
import Foreign.Ptr

import System.Environment

main = do
    [fl] <- getArgs
    g <- readAdjMatrix fl :: IO (LGraph U B.ByteString ())
    print $ (map.map) (flip vertexLab g) $ maximalCliques (0,0) g
    print $ (map.map) (flip vertexLab g) $ communityLeadingEigenvector g (const Nothing) 1000

