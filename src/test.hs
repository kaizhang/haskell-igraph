{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Foreign hiding (new)
import Control.Monad
import Data.Serialize
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import IGraph
import IGraph.Read
import Text.XML.HXT.Core
import IGraph.Export.GEXF
import System.Environment

main = do
--    [fl] <- getArgs
--    g <- readAdjMatrix fl :: IO (LGraph U B.ByteString Double)
    let t = genXMLTree undefined :: IOStateArrow s XmlTree XmlTree
    [x] <- runX $ root [] [t] >>> writeDocumentToString [withXmlPi yes, withIndent yes]
    putStrLn x
