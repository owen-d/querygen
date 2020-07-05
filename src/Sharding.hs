module Sharding
        ()
where

import           Lang
shardLabel = "__cortex_shard__"

data Shard = Shard Int Int

instance Show Shard where
  show (Shard a b) = show $ toLabel a b
    where
      toLabel a b = Selector shardLabel MatchEquals $ (show a) ++ "_of_" ++ (show b)
