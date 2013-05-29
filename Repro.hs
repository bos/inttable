module Main (main) where

import qualified Data.Vector as V
import qualified IntMap as I

constructMap :: V.Vector (Int, [Int]) -> I.IntMap [Int]
constructMap = V.foldl' go I.empty
    where go m (k,v) = snd $ I.insertWith (++) k v m

main = constructMap V.empty `seq` return ()
