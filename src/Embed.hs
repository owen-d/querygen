{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Embed where

import qualified Data.Aeson as A
import Data.Aeson
  ( ToJSON,
    (.=),
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
data Merger a = Concat a

instance ToJSON a => ToJSON (Merger a) where
  toJSON (Concat x) = A.object ["Concat" .= (toJSON x)]
