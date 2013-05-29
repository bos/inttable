module Main (main) where

import Control.Applicative
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified IntMap as I
import Data.Vector (Vector)
import IntMap (IntMap)
import System.Random.MWC

constructMap :: Vector (Int, [Int]) -> IntMap [Int]
constructMap = G.foldl' go I.empty
    where go m (k,v) = case I.insertWith (++) k v m of
                         (old, m') -> old `seq` m'

genKVs :: Int -> IO (Vector (Int, [Int]))
genKVs count = do
  gen <- create
  G.replicateM count $ (flip (,) [1]) <$> uniformR (0, count) gen

main = do
  kvs <- genKVs 256
  defaultMain [
    bgroup "construct" [
        bench "intmap" $ whnf constructMap kvs
      ]
    ]
