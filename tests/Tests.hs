{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.IntMap (IntMap)
import Data.Word (Word8)
import IntTable (IntTable)
import System.IO.Unsafe
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.IntMap as IM
import qualified IntTable as IT

type V = [Word8]
type KV = (Int, V)

newtype IT = IT { fromIT :: IntTable V }

fromList :: [KV] -> IT
fromList = IT . unsafePerformIO . IT.fromList

toList :: IT -> [KV]
toList = unsafePerformIO . IT.toList . fromIT

instance Arbitrary IT where
    arbitrary = fromList <$> arbitrary
    shrink = map fromList . shrink . toList

same :: [KV] -> IntMap V -> IntTable V -> IO Bool
same kvs im it = do
  itr <- mapM (\(k,_) -> IT.lookup k it) kvs
  itsize <- IT.getSize it
  return $! map (\(k,_) -> IM.lookup k im) kvs == itr &&
            itsize == IM.size im

t_fromList :: [KV] -> Bool
t_fromList kvs = unsafePerformIO $
  same kvs (IM.fromList kvs) =<< IT.fromList kvs

t_insertWith :: Int -> V -> [KV] -> Bool
t_insertWith k v kvs = unsafePerformIO $ do
  let im = IM.fromList kvs
  it <- IT.fromList kvs
  ov0 <- IT.lookup k it
  ov1 <- IT.insertWith (++) k v it
  (ov0 == ov1 &&) <$> same kvs (IM.insertWith (++) k v im) it

t_deleteWith :: (Int -> V -> IntMap V -> IntMap V)
             -> (Int -> V -> IntTable V -> IO b)
             -> Int -> V -> [KV] -> Bool
t_deleteWith f g k v kvs = unsafePerformIO $ do
  let im0 = f k v . IM.fromList $ kvs
      (v0, im) = (IM.lookup k im0, IM.delete k im0)
  it <- IT.fromList kvs
  _ <- g k v it
  v1 <- IT.delete k it
  (v0 == v1 &&) <$> same kvs im it

t_delete_present :: Int -> V -> [KV] -> Bool
t_delete_present = t_deleteWith (IM.insertWith (++)) (IT.insertWith (++))

t_delete_missing :: Int -> V -> [KV] -> Bool
t_delete_missing = t_deleteWith (const . IM.delete) (const . IT.delete)

t_updateWith :: Int -> V -> [KV] -> Bool
t_updateWith k v kvs  = unsafePerformIO $ do
  let im0             = IM.fromList kvs
      im1 | even k    = im0
          | otherwise = IM.insert k v im0
      (v0, im)        = (IM.lookup k im1, IM.update f k im1)
      f (x:xs) | odd x     = Nothing
               | otherwise = Just (x `rotate` 3 : xs)
      f _                  = Just []
  it <- IT.fromList kvs
  when (odd k) .
    void $ IT.insertWith const k v it
  v1 <- IT.updateWith f k it
  (v0 == v1 &&) <$> same kvs im it

main :: IO ()
main = defaultMain properties

properties :: [Test]
properties = [
    testProperty "fromList" t_fromList
  , testProperty "insertWith" t_insertWith
  , testProperty "delete_present" t_delete_present
  , testProperty "delete_missing" t_delete_missing
  , testProperty "updateWith" t_updateWith
  ]
