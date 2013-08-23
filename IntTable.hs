{-# LANGUAGE BangPatterns, NoImplicitPrelude, RecordWildCards, Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module IntTable
    (
    -- * Map type
      IntTable

    -- * Query
    , lookup
    , toList
    , getSize

    -- * Construction
    , new
    , fromList

    -- * Insertion
    , insertWith

    -- * Delete\/Update
    , reset
    , delete
    , updateWith
    ) where

import Arr (Arr)
import Control.Monad ((=<<), forM_, liftM, unless, when)
import Data.Bits ((.&.), shiftL, shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..), isJust)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Storable (peek, poke)
import GHC.Base (Monad(..), ($), const, flip, otherwise)
import GHC.Classes (Eq(..), Ord(..))
import GHC.Num (Num(..))
import GHC.Prim (seq)
import GHC.Types (Bool(..), IO(..), Int(..))
import qualified Arr

-- A very simple chained integer-keyed mutable hash table. We use
-- power-of-two sizing, grow at a load factor of 0.75, and never
-- shrink. The "hash function" is the identity function.

newtype IntTable a = IntTable (IORef (IT a))

data IT a = IT {
      tabArr  :: {-# UNPACK #-} !(Arr (Bucket a))
    , tabSize :: {-# UNPACK #-} !(ForeignPtr Int)
    }

data Bucket a = Empty
              | Bucket {
      bucketKey   :: {-# UNPACK #-} !Int
    , bucketValue :: a
    , bucketNext  :: Bucket a
    }

lookup :: Int -> IntTable a -> IO (Maybe a)
lookup k (IntTable ref) = do
  let go Bucket{..}
        | bucketKey == k = return (Just bucketValue)
        | otherwise      = go bucketNext
      go _ = return Nothing
  it@IT{..} <- readIORef ref
  go =<< Arr.read tabArr (indexOf k it)

new :: Int -> IO (IntTable a)
new capacity = IntTable `liftM` (newIORef =<< new_ capacity)

new_ :: Int -> IO (IT a)
new_ capacity = do
  arr <- Arr.new Empty capacity
  size <- mallocForeignPtr
  withForeignPtr size $ \ptr -> poke ptr 0
  return IT { tabArr = arr
            , tabSize = size
            }

grow :: IT a -> IORef (IT a) -> Int -> IO ()
grow oldit ref size = do
  newit <- new_ (Arr.size (tabArr oldit) `shiftL` 1)
  let copySlot n !i
        | n == size = return ()
        | otherwise = do
          let copyBucket !m Empty          = copySlot m (i+1)
              copyBucket  m bkt@Bucket{..} = do
                let idx = indexOf bucketKey newit
                next <- Arr.read (tabArr newit) idx
                Arr.write (tabArr newit) idx bkt { bucketNext = next }
                copyBucket (m+1) bucketNext
          copyBucket n =<< Arr.read (tabArr oldit) i
  copySlot 0 0
  withForeignPtr (tabSize newit) $ \ptr -> poke ptr size
  writeIORef ref newit

insertWith :: (a -> a -> a) -> Int -> a -> IntTable a -> IO (Maybe a)
insertWith f k v inttable@(IntTable ref) = do
  it@IT{..} <- readIORef ref
  let idx = indexOf k it
      go seen bkt@Bucket{..}
        | bucketKey == k = do
          let !v' = f v bucketValue
              !next = seen <> bucketNext
              Empty        <> bs = bs
              b@Bucket{..} <> bs = b { bucketNext = bucketNext <> bs }
          Arr.write tabArr idx (Bucket k v' next)
          return (Just bucketValue)
        | otherwise = go bkt { bucketNext = seen } bucketNext
      go seen _ = withForeignPtr tabSize $ \ptr -> do
        size <- peek ptr
        if size + 1 >= Arr.size tabArr - (Arr.size tabArr `shiftR` 2)
          then grow it ref size >> insertWith f k v inttable
          else do
            v `seq` Arr.write tabArr idx (Bucket k v seen)
            poke ptr (size + 1)
            return Nothing
  go Empty =<< Arr.read tabArr idx
{-# INLINABLE insertWith #-}

getSize :: IntTable a -> IO Int
getSize (IntTable ref) = do
  IT{..} <- readIORef ref
  withForeignPtr tabSize peek

-- | Used to undo the effect of a prior insertWith.
reset :: Int -> Maybe a -> IntTable a -> IO ()
reset k (Just v) tbl = insertWith (flip const) k v tbl >> return ()
reset k Nothing  tbl = delete k tbl >> return ()

toList :: IntTable a -> IO [(Int, a)]
toList (IntTable ref) = do
  IT{..} <- readIORef ref
  let outer acc i
        | i >= Arr.size tabArr = return acc
        | otherwise = do
          let inner iacc Empty = outer iacc (i+1)
              inner iacc Bucket{..} =
                inner ((bucketKey,bucketValue):iacc) bucketNext
          inner acc =<< Arr.read tabArr i
  outer [] 0

fromList :: [(Int, a)] -> IO (IntTable a)
fromList kvs = do
  t <- new 64
  forM_ kvs $ \(k,v) -> insertWith const k v t
  return t

indexOf :: Int -> IT a -> Int
indexOf k IT{..} = k .&. (Arr.size tabArr - 1)

delete :: Int -> IntTable a -> IO (Maybe a)
delete k t = updateWith (const Nothing) k t

updateWith :: (a -> Maybe a) -> Int -> IntTable a -> IO (Maybe a)
updateWith f k (IntTable ref) = do
  it@IT{..} <- readIORef ref
  let idx = indexOf k it
      go changed bkt@Bucket{..}
        | bucketKey == k =
            let fbv = f bucketValue
                !nb = case fbv of
                        Just val -> bkt { bucketValue = val }
                        Nothing  -> bucketNext
            in (fbv, Just bucketValue, nb)
        | otherwise = case go changed bucketNext of
                        (fbv, ov, nb) -> (fbv, ov, bkt { bucketNext = nb })
      go _ e = (Nothing, Nothing, e)
  (fbv, oldVal, newBucket) <- go False `liftM` Arr.read tabArr idx
  when (isJust oldVal) $ do
    Arr.write tabArr idx newBucket
    unless (isJust fbv) $
      withForeignPtr tabSize $ \ptr -> do
        size <- peek ptr
        poke ptr (size - 1)
  return oldVal
