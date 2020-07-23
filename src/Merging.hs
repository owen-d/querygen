{-# LANGUAGE OverloadedStrings #-}

module Merging where

import qualified Data.Aeson as A
import Data.Aeson
  ( (.=),
    ToJSON,
    toJSON,
  )


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

type Labels = [Label]

data Series = Series {labels :: Labels, samples :: Samples}
