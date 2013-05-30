{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified IntMap as IM
import qualified IntTable as IT
import Data.Vector (Vector)
import IntMap (IntMap)
import IntTable (IntTable)
import System.Random.MWC
import Data.Bits

mkMap = G.foldl' go IM.empty
    where go m (k,v) = snd $ IM.insertWith (+) k v m

mkTable kvs = do
  t <- IT.new 16
  G.forM_ kvs $ \(k,v) -> IT.insertWith (+) k v t
  return t

update v
    | v .&. 1 == 0 = Nothing
    | otherwise    = Just $! v+1

updateMap m = G.foldl' go m
  where go m k = snd $ IM.updateWith update k m

updateTable t ks = G.forM_ ks $ \k -> IT.updateWith update k t

deleteMap m = G.foldl' go m
  where go m k = snd $ IM.delete k m

deleteTable t ks = G.forM_ ks $ \k -> IT.delete k t

genKVs :: Int -> IO (Vector (Int, Int))
genKVs count = do
  gen <- create
  G.replicateM count $ (flip (,) 1) <$> uniformR (0, count) gen

main = do
  let sizes = [16, 64, 256, 1024]
  inputs <- forM sizes $ \size -> do
              kvs <- genKVs size
              let !m = mkMap kvs
              t <- mkTable kvs
              return (size, kvs, m, t)
  let benches (size, kvs, m, t) = [
          bgroup "construct" [ bgroup (show size) [
            bench "map" $ whnf mkMap kvs
          , bench "table" $ whnfIO (mkTable kvs)
          ] ]
        , let ks = G.map fst . G.take (G.length kvs `div` 2) $ kvs
          in bgroup "update" [ bgroup (show size) [
               bench "map" $ whnf (updateMap m) ks
             , bench "table" $ whnfIO (updateTable t ks)
             ] ]
        , let ks = G.map fst . G.take (G.length kvs `div` 2) $ kvs
          in bgroup "delete" [ bgroup (show size) [
               bench "map" $ whnf (deleteMap m) ks
             , bench "table" $ whnfIO (deleteTable t ks)
             ] ]
        ]
  defaultMain $ concatMap benches inputs
