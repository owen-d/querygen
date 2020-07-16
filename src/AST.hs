{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AST where

import Lang (Aggregator (..), Grouping (..), Metric (..), Operator (..), TimeRange (..))
import Sharding

data InstantVector = Aggregator Aggregator Grouping InstantVector | MetricVec Metric | Scalar Int

data Function a b = Vector Int | Round a (Maybe Int) | Sqrt a | Floor a | Ceil a | Rate b | Ln a | Increase b

data RangeVector a = MetricRange Metric TimeRange | SubQuery a TimeRange TimeRange

class BinaryOp a b c | a b -> c where
  binOp :: b -> a -> a -> c

instance BinaryOp InstantVector Operator InstantVector where
  binOp _ _ _ = undefined

instance BinaryOp ShardIV Operator ShardIV where
  binOp _ _ _ = undefined

data AST a b c where
  Leaf :: a -> AST a b c
  Aggregation :: Aggregator -> a -> AST a b c
  Function :: Function a c -> AST a b c
  BinOp :: BinaryOp a b a => a -> b -> a -> AST a b c

-- | PromQL is an alias for the AST representation of promql without shard annotations
type PromQL = AST InstantVector Operator (RangeVector InstantVector)

-- | ShardIV is a shard-aware instant vector
data ShardIV = ShardIV Shard InstantVector

-- | ShardQL is an alias for the AST representation of promql with shard annotations
type ShardQL = AST ShardIV Operator (RangeVector ShardIV)
