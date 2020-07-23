{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Embed where

import Data.Aeson
  ( ToJSON,
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
      ls = Matchers [Selector queryLabel MatchEquals (show . encode . toJSON . decode $ x)]
