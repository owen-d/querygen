module Sharding where

import Lang

shardLabel :: [Char]
shardLabel = "__cortex_shard__"

data Shard = Shard Int Int

instance Show Shard where
  show = show . labels

instance HasLabels Shard where
  labels (Shard a b) = Labels [Selector shardLabel MatchEquals $ (show a) ++ "_of_" ++ (show b)]
