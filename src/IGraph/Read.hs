module IGraph.Read where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lex.Double (readDouble)
import Data.Maybe

import IGraph

readAdjMatrix :: Graph gr d => FilePath -> IO (gr d B.ByteString ())
readAdjMatrix fl = do
    c <- B.readFile fl
    let (header:xs) = B.lines c
        mat = map (map (fst . fromJust . readDouble) . B.words) xs
        es = fst $ unzip $ filter f $ zip [ (i,j) | i <- [0..nrow-1], j <- [0..nrow-1] ] $ concat mat
        nrow = length mat
        ncol = length $ head mat
    if nrow /= ncol
       then error "nrow != ncol"
       else return $ mkGraph (nrow, Just $ B.words header) (es, Nothing)
  where
    f ((i,j),v) = i /= j && v /= 0

readAdjMatrixWeighted :: Graph gr d => FilePath -> IO (gr d B.ByteString Double)
readAdjMatrixWeighted fl = do
    c <- B.readFile fl
    let (header:xs) = B.lines c
        mat = map (map (fst . fromJust . readDouble) . B.words) xs
        (es, ws) = unzip $ filter f $ zip [ (i,j) | i <- [0..nrow-1], j <- [0..nrow-1] ] $ concat mat
        nrow = length mat
        ncol = length $ head mat
    if nrow /= ncol
       then error "nrow != ncol"
       else return $ mkGraph (nrow, Just $ B.words header) (es, Just ws)
  where
    f ((i,j),v) = i /= j && v /= 0
