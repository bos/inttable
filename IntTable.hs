{-# LANGUAGE BangPatterns, MagicHash, NoImplicitPrelude, RecordWildCards, UnboxedTuples #-}

module IntTable
    (
    -- * Map type
      IntTable
    , Key

    -- * Query
    , lookup
    , toList

    -- * Construction
    , new
    , fromList

    -- * Insertion
    , insertWith

    -- * Delete\/Update
    , delete
    , updateWith
    ) where

import Data.Maybe (Maybe(..))
import GHC.Base (Monad(..), ($), const, otherwise)
import GHC.Classes (Eq(..), Ord(..))
import GHC.Num (Num(..))
import GHC.Prim (MutableArray#, RealWorld, newArray#, readArray#, seq, sizeofMutableArray#, writeArray#)
import GHC.Types (Bool(..), IO(..), Int(..))
import GHC.Err (undefined)
import Data.IORef
import Data.Bits
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad ((=<<), liftM, forM_)

newtype IntTable a = IntTable (IORef (IT a))

data IT a = IT {
      tabArr  :: Arr (Bucket a)
    , tabSize :: ForeignPtr Int
    }

data Arr a = Arr (MutableArray# RealWorld a)

data Bucket a = Empty
              | Bucket {
      bucketKey   :: {-# UNPACK #-} !Key
    , bucketValue :: a
    , bucketNext  :: Bucket a
    }

type Key = Int

lookup :: Key -> IntTable a -> IO (Maybe a)
lookup k (IntTable ref) = do
  let go Bucket{..}
        | bucketKey == k = return (Just bucketValue)
        | otherwise      = go bucketNext
      go _ = return Nothing
  it@IT{..} <- readIORef ref
  go =<< readArr tabArr (indexOf k it)

new :: Int -> IO (IntTable a)
new capacity = IntTable `liftM` (newIORef =<< new_ capacity)

new_ :: Int -> IO (IT a)
new_ capacity = do
  arr <- newArr Empty capacity
  size <- mallocForeignPtr
  withForeignPtr size $ \ptr -> poke ptr 0
  return IT { tabArr = arr
            , tabSize = size
            }

(<>) :: Bucket a -> Bucket a -> Bucket a
Empty        <> bs = bs
b@Bucket{..} <> bs = b { bucketNext = bucketNext <> bs }
infixr 5 <>

grow :: IT a -> IORef (IT a) -> Int -> IO ()
grow oldit ref size = do
  newit <- new_ (sizeArr (tabArr oldit) `shiftL` 1)
  let copyOuter n !i
        | n == size = return ()
        | otherwise = do
          let inner !m Empty         = copyOuter m (i+1)
              inner m bkt@Bucket{..} = do
                let idx = indexOf bucketKey newit
                next <- readArr (tabArr newit) idx
                writeArr (tabArr newit) idx bkt { bucketNext = next }
                inner (m+1) bucketNext
          inner n =<< readArr (tabArr oldit) i
  copyOuter 0 0
  withForeignPtr (tabSize newit) $ \ptr -> poke ptr size
  writeIORef ref newit

insertWith :: (a -> a -> a) -> Key -> a -> IntTable a -> IO (Maybe a)
insertWith f k v inttable@(IntTable ref) = do
  it@IT{..} <- readIORef ref
  let idx = indexOf k it
  let go seen bkt@Bucket{..}
        | bucketKey == k = do
          let !v' = f bucketValue v
          writeArr tabArr idx (Bucket k v' (seen <> bucketNext))
          return (Just bucketValue)
        | otherwise = go bkt { bucketNext = seen } bucketNext
      go seen _ = withForeignPtr tabSize $ \ptr -> do
        size <- peek ptr
        if size + 1 >= sizeArr tabArr `shiftR` 1
          then grow it ref size >> insertWith f k v inttable
          else do
            v `seq` writeArr tabArr idx (Bucket k v seen)
            poke ptr (size + 1)
            return Nothing
  go Empty =<< readArr tabArr idx
{-# INLINABLE insertWith #-}

toList :: IntTable a -> IO [(Key, a)]
toList (IntTable ref) = do
  IT{..} <- readIORef ref
  let outer acc i
        | i >= sizeArr tabArr = return acc
        | otherwise = do
          let inner iacc Empty = outer iacc (i+1)
              inner iacc Bucket{..} =
                inner ((bucketKey,bucketValue):iacc) bucketNext
          inner acc =<< readArr tabArr i
  outer [] 0

fromList :: [(Key, a)] -> IO (IntTable a)
fromList kvs = do
  t <- new 64
  forM_ kvs $ \(k,v) -> insertWith const k v t
  return t

indexOf :: Key -> IT a -> Int
indexOf k IT{..} = k .&. (sizeArr tabArr - 1)

delete :: Key -> IntTable a -> IO (Maybe a)
delete = undefined

updateWith :: (a -> Maybe a) -> Key -> IntTable a -> IO (Maybe a)
updateWith f k (IntTable ref) = do
  it@IT{..} <- readIORef ref
  let idx = indexOf k it
      go changed bkt@Bucket{..}
        | bucketKey == k = (Just bucketValue,
                            case f bucketValue of
                              Just val -> bkt { bucketValue = val }
                              Nothing -> bucketNext)
        | otherwise = case go changed bucketNext of
                        (ov, nb) -> (ov, bkt { bucketNext = nb })
      go _ e = (Nothing, e)
  (oldVal, newBucket) <- go False `liftM` readArr tabArr idx
  case oldVal of
    Just _ -> writeArr tabArr idx newBucket >> return oldVal
    _      -> return oldVal

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
