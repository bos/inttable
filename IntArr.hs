{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}

module IntArr
    (
      IntArr
    , new
    , read
    , write
    ) where

import Foreign.Storable (sizeOf)
import GHC.Base (($))
import GHC.Types (IO(..), Int(..))
import GHC.Num (Num(..))
import GHC.Prim (MutableByteArray#, RealWorld, newByteArray#, readIntArray#,
                 writeIntArray#)

data IntArr = Arr (MutableByteArray# RealWorld)

new :: Int -> IO IntArr
new n = IO $ \s0# ->
  case n * sizeOf n of
    I# n# -> case newByteArray# n# s0# of
               (# s1#, mba# #) -> (# s1#, Arr mba# #)

read :: IntArr -> Int -> IO Int
read (Arr mba#) (I# n#) = IO $ \s0# ->
  case readIntArray# mba# n# s0# of
    (# s1#, v# #) -> (# s1#, I# v# #)

write :: IntArr -> Int -> Int -> IO ()
write (Arr mba#) (I# n#) (I# v#) = IO $ \s0# ->
  case writeIntArray# mba# n# v# s0# of
    s1# -> (# s1#, () #)
