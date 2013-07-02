{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative
import Data.Bits
import Data.IntMap (IntMap)
import Data.Word (Word8)
import IntTable (IntTable, Key)
import System.IO.Unsafe
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.IntMap as IM
import qualified IntTable as IT

type V = Word8
type KV = (Key, V)

newtype IT = IT { fromIT :: IntTable V }

type IM = IntMap V

fromList :: [KV] -> IT
fromList = IT . unsafePerformIO . IT.fromList

toList :: IT -> [KV]
toList = unsafePerformIO . IT.toList . fromIT

instance Arbitrary IT where
    arbitrary = fromList <$> arbitrary
    shrink = map fromList . shrink . toList

t_fromList :: [KV] -> Bool
t_fromList kvs = map (\(k,_) -> IM.lookup k im) kvs ==
                 unsafePerformIO (mapM (\(k,_) -> IT.lookup k it) kvs)
  where IT it = fromList kvs
        im = IM.fromList kvs

t_insertWith :: Key -> V -> [KV] -> Bool
t_insertWith k v kvs =
    IM.lookup k (IM.insertWith f k v im) ==
    unsafePerformIO (IT.insertWith f k v it >> IT.lookup k it)
  where IT it = fromList kvs
        im = IM.fromList kvs
        f a b = (a `xor` b) `rotate` 3

main :: IO ()
main = defaultMain properties

properties :: [Test]
properties = [
    testProperty "fromList" t_fromList
  , testProperty "insertWith" t_insertWith
  ]
