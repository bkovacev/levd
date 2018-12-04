-- Common.hs
-- Useful functions that can be used in a general situations

module Common where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Returns value if found or closest match
-- Element in range [first ,last) that is non less then (i.e. >=) to value, or LAST if nothing found
lowerbound :: (Ord k) => Map k a -> k -> Maybe a
lowerbound dict t
  | Map.null dict = Nothing
  | otherwise = let init = (False, (t, snd (Map.findMax dict)))
                in Just (snd (snd (Map.foldrWithKey fn init dict)))
  where fn k v acc@(True,_) = acc
        fn k v (False, (t, old))
          | k >= t     = (False, (t, v))
          | otherwise = (True, (t, old))

-- Sorts a tuple list
sortTupleListByFirst :: (Ord a) => [(a, b)] -> [(a, b)]
sortTupleListByFirst xs = sortBy (\x y -> compare (fst x) (fst y)) xs
