module IGraph.Read
    ( readAdjMatrix
    , fromAdjMatrix
    , readAdjMatrixWeighted
    ) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lex.Fractional (readSigned, readExponential)
import Data.Maybe (fromJust)

import IGraph

readDouble :: B.ByteString -> Double
readDouble = fst . fromJust . readSigned readExponential
{-# INLINE readDouble #-}

readAdjMatrix :: Graph d => FilePath -> IO (LGraph d B.ByteString ())
readAdjMatrix = fmap fromAdjMatrix . B.readFile

fromAdjMatrix :: Graph d => B.ByteString -> LGraph d B.ByteString ()
fromAdjMatrix bs =
    let (header:xs) = B.lines bs
        mat = map (map readDouble . B.words) xs
        es = fst $ unzip $ filter f $ zip [ (i,j) | i <- [0..nrow-1], j <- [0..nrow-1] ] $ concat mat
        nrow = length mat
        ncol = length $ head mat
    in if nrow /= ncol
         then error "fromAdjMatrix: nrow != ncol"
         else mkGraph (B.words header) $ zip es $ repeat ()
  where
    f ((i,j),v) = i < j && v /= 0
{-# INLINE fromAdjMatrix #-}

readAdjMatrixWeighted :: Graph d => FilePath -> IO (LGraph d B.ByteString Double)
readAdjMatrixWeighted fl = do
    c <- B.readFile fl
    let (header:xs) = B.lines c
        mat = map (map readDouble . B.words) xs
        (es, ws) = unzip $ filter f $ zip [ (i,j) | i <- [0..nrow-1], j <- [0..nrow-1] ] $ concat mat
        nrow = length mat
        ncol = length $ head mat
    if nrow /= ncol
       then error "nrow != ncol"
       else return $ mkGraph (B.words header) $ zip es ws
  where
    f ((i,j),v) = i < j && v /= 0
