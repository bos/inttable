{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}

module Arr
    (
      Arr(..)
    , newArr
    , sizeArr
    , readArr
    , writeArr
    ) where

import GHC.Base (($))
import GHC.Prim (MutableArray#, RealWorld, newArray#, readArray#, sizeofMutableArray#, writeArray#)
import GHC.Types (IO(..), Int(..))

data Arr a = Arr (MutableArray# RealWorld a)

newArr :: a -> Int -> IO (Arr a)
newArr defval (I# n#) = IO $ \s0# ->
  case newArray# n# defval s0# of (# s1#, marr# #) -> (# s1#, Arr marr# #)

sizeArr :: Arr a -> Int
sizeArr (Arr a) = I# (sizeofMutableArray# a)

readArr :: Arr a -> Int -> IO a
readArr (Arr a) (I# n#) = IO $ \s0# ->
  case readArray# a n# s0# of (# s1#, val #) -> (# s1#, val #)

writeArr :: Arr a -> Int -> a -> IO ()
writeArr (Arr a) (I# n#) val = IO $ \s0# ->
  case writeArray# a n# val s0# of s1# -> (# s1#, () #)
