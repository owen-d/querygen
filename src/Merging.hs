{-# LANGUAGE OverloadedStrings #-}

module Merging where

import qualified Data.Aeson as A
import Data.Aeson
  ( (.=),
    ToJSON,
    toJSON,
  )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Merger is a type for how to merge different legs of queries
-- Concat denotes that the resulting queries should have their results concatenated together
-- Noop denotes no special merging should take place
data Merger a = Concat a | Noop a

instance ToJSON a => ToJSON (Merger a) where
  toJSON x = A.object [key .= (toJSON v)]
    where
      (key, v) = case x of
        Concat y -> ("Concat", y)
        Noop y -> ("Noop", y)

-- In order to tell whether an operation can be parallelized, two things must be true:
-- (1): The operation on the samples must be associative.
-- (2): The grouping parameters applied over (1) must be associative
-- Need to test:
-- (1) Merging Aggregations
-- (2) Merging binary operations
-- (3) Merging functions
-- (4) recursive merging of the above

data Sample = Sample {ts :: Float, sVal :: Float}

type Samples = [Sample]

data Label = Label {lName :: String, lValue :: String}
  deriving (Eq, Ord)

type Labels = [Label]

data Series = Series {labels :: Labels, samples :: Samples}

class Monoid a => SeriesMerger a where
  merge :: a -> [Series]

mergeSeries :: SeriesMerger a => (Series -> a) -> [Series] -> [Series]
mergeSeries f xs = merge . mconcat $ fmap f xs

-- Merge operations

data Sum = Sum (Map Labels Sample)

instance Semigroup Sum where
  (Sum x) <> (Sum y) = Sum $ Map.unionWith fn x y
    where
      fn a b = Sample {ts = ts a, sVal = sVal a + sVal b}

instance Monoid Sum where
  mempty = Sum Map.empty

instance SeriesMerger Sum where
  merge (Sum x) = map fn $ Map.toList x
    where fn (k,v) = Series {labels=k, samples=[v]}
