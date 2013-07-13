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

mkMap' m0 = G.foldl' go m0
    where go m (k,v) = snd $ IM.insertWith (+) k v m

mkMap = mkMap' IM.empty

mkTable' t kvs = do
  G.forM_ kvs $ \(k,v) -> IT.insertWith (+) k v t
  return t

mkTable kvs = IT.new 16 >>= flip mkTable' kvs

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

slowMap kvs0 kvs1 ks = loop 100 (mkMap kvs0)
  where loop n !m | n == 0 = m
                  | otherwise = loop (n-1) $! deleteMap (updateMap (mkMap' m kvs1) ks) ks

slowTable kvs0 kvs1 ks = do
  t <- mkTable kvs0
  let loop n | n == 0 = return ()
             | otherwise = do
        mkTable' t kvs1
        updateTable t ks
        deleteTable t ks
        loop (n-1)
  loop 100

main = do
  let sizes = [16, 64, 256, 1024]
  inputs <- forM sizes $ \size -> do
              kvs0 <- genKVs size
              kvs1 <- genKVs size
              let !m = mkMap kvs0
              t <- mkTable kvs0
              return (size, kvs0, kvs1, m, t)
  let benches (size, kvs0, kvs1, m, t) = [
          bgroup "construct" [ bgroup (show size) [
            bench "map" $ whnf mkMap kvs0
          , bench "table" $ whnfIO (mkTable kvs0)
          ] ]
        , let ks = G.map fst . G.take (G.length kvs0 `div` 2) $ kvs0
          in bgroup "update" [ bgroup (show size) [
               bench "map" $ whnf (updateMap m) ks
             , bench "table" $ whnfIO (mkTable kvs0 >>= flip updateTable ks)
             ] ]
        , let ks = G.map fst . G.take (G.length kvs0 `div` 2) $ kvs0
          in bgroup "delete" [ bgroup (show size) [
               bench "map" $ whnf (deleteMap m) ks
             , bench "table" $ whnfIO (mkTable kvs0 >>= flip deleteTable ks)
             ] ]
        , let ks = G.map fst kvs1
          in bgroup "slow" [ bgroup (show size) [
               bench "map" $ whnf (slowMap kvs0 kvs1) ks
             , bench "table" $ whnfIO (slowTable kvs0 kvs1 ks)
             ] ]
        ]
  defaultMain $ concatMap benches inputs
