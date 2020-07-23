{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sharding where

import AST
import Lang


shardLabel :: [Char]
shardLabel = "__cortex_shard__"

data Shard = Shard Int Int

instance Show Shard where
  show = show . matchers

instance HasMatchers Shard where
  matchers (Shard a b) = Matchers [Selector shardLabel MatchEquals $ (show a) ++ "_of_" ++ (show b)]

instance BinaryOp ShardIV Operator ShardIV where
  binOp _ _ _ = undefined

-- | ShardIV is a shard-aware instant vector
data ShardIV = ShardIV Shard InstantVector

-- | ShardQL is an alias for the AST representation of promql with shard annotations
type ShardQL = AST ShardIV Operator (RangeVector ShardIV)
