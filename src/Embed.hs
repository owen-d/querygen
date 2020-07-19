{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Embed where

import qualified Data.Aeson as A
import Data.Aeson
  ( (.=),
    ToJSON,
    encode,
    toJSON,
  )

import Lang
queryLabel :: String
queryLabel = "__cortex_queries__"

embeddedMetric :: String
embeddedMetric = "__embedded_queries__"

data Encoded a = Encoded {decode :: a}

instance ToJSON a => Show (Encoded a) where
  show x = show $ MetricVec (Metric embeddedMetric ls)
    where
      ls = Labels [Selector queryLabel MatchEquals (show . encode . toJSON . decode $ x)]

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
