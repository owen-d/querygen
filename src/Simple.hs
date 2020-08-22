{-# LANGUAGE OverloadedStrings #-}

module Simple where

import qualified Data.Map.Strict as M
import Merging (Label (..), Labels (..), Sample (..), Samples, Series (..), SeriesMerger (..), Sum (..))
import Data.Hashable (Hashable, hash)

shards :: Hashable a => Int -> [a] -> [[a]]
shards factor xs =
  filter notEmpty $ map snd (M.toList m)
  where
    notEmpty x = null x == False
    m = M.fromListWith (<>) (pairs xs)
    pairs = map $ \x -> (hash x `mod` factor, [x])

sset :: [Series]
sset = map ls [[("foo", "bar")], [("bazz", "buzz")]]
  where
    ls pairs = Series {labels = Labels lset, samples = []}
      where
        lset = map (\(x, y) -> Label x y) pairs
