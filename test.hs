import Foreign hiding (new)
import Control.Monad
import Data.Serialize
import qualified Data.ByteString.Internal as B
import IGraph
import IGraph.Internal.Graph
import IGraph.Internal.Generator
import IGraph.Internal.Attribute
import IGraph.Internal.Initialization
import Foreign.Ptr

main = do
    let g = new 5 :: LGraph U String Double
    addLEdges "weight" [(1,2,1.1234),(3,4,pi)] g
    print $ igraphCattributeHasAttr (_graph g) 2 "weight"
    let s = igraphCattributeEAS (_graph g) "weight" 1
    print $ (read s :: Double)
