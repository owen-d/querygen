{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AST where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lang (AggOp (..), Grouping (..), Labels (..), MatchType (..), Metric (..), Operator (..), Selector (..), TimeRange (..), TwoArityAggOp (..))
import Sharding

data InstantVector = MetricVec Metric | Scalar Int

instance Show InstantVector where
  show (Scalar v) = show v
  show (MetricVec m) = show m

data Aggregator a = Aggregator AggOp Grouping a

instance Show a => Show (Aggregator a) where
  show (Aggregator agg grp vec) = case agg of
    TwoArityAggOp x ->
      concat
        [show x, show grp, " (", v, ",", show vec, ")"]
      where
        v = case x of
          Bottomk v' -> show v'
          Topk v' -> show v'
          Quantile v' -> show v'
    y -> concat [show y, show grp, " (", show vec, ")"]

data Function a = Vector Int | Round a (Maybe Int) | Sqrt a | Floor a | Ceil a | Rate (RangeVector a) | Ln a | Increase (RangeVector a)

instance (Show a) => Show (Function a) where
  show x = concat [fn, "(", innards, ")"]
    where
      (fn, innards) = case x of
        Vector x' -> ("vector", show x')
        Round x' y ->
          ( "round",
            concat [show x', show (fromMaybe 1 y)]
          )
        Sqrt v -> ("sqrt", show v)
        Floor v -> ("floor", show v)
        Ceil v -> ("ceil", show v)
        Rate r -> ("rate", show r)
        Ln v -> ("ln", show v)
        Increase r -> ("increase", show r)

data RangeVector a = MetricRange Metric TimeRange | SubQuery a TimeRange TimeRange

instance Show a => Show (RangeVector a) where
  show x = case x of
    MetricRange m t -> concat [show m, "[", show t, "]"]
    SubQuery vec range resolution ->
      concat
        [ show vec,
          "[",
          show range,
          ":",
          show resolution,
          "]"
        ]

class BinaryOp a b c | a b -> c where
  binOp :: b -> a -> a -> c

instance BinaryOp InstantVector Operator InstantVector where
  binOp _ _ _ = undefined

instance BinaryOp ShardIV Operator ShardIV where
  binOp _ _ _ = undefined

-- | AST is a promql-like AST abstracted over three type variables:
-- a (instant vector type)
-- b (binary operation type, used via functional dependencies to ensure two merged instant vectors result in another instant vector)
-- c (range vector type)
data AST a b c where
  Leaf :: a -> AST a b c
  Aggregation :: Aggregator (AST a b c) -> AST a b c
  Function :: Function (AST a b c) -> AST a b c
  BinOp :: BinaryOp a b a => (AST a b c) -> b -> (AST a b c) -> AST a b c

instance (Show a, Show b, Show c) => Show (AST a b c) where
  show (Leaf x) = show x
  show (Aggregation x) = show x
  show (Function x) = show x
  show (BinOp vec1 op vec2) = intercalate " " [show vec1, show op, show vec2]

-- | PromQL is an alias for the AST representation of promql without shard annotations
type PromQL = AST InstantVector Operator (RangeVector InstantVector)

-- | ShardIV is a shard-aware instant vector
data ShardIV = ShardIV Shard InstantVector

-- | ShardQL is an alias for the AST representation of promql with shard annotations
type ShardQL = AST ShardIV Operator (RangeVector ShardIV)

qry :: PromQL
qry = Aggregation $ Aggregator Sum groupBy rate
  where
    groupBy = Grouping {groups = ["bazz", "borg"], without = False}
    rate = Function $ Rate $ MetricRange metric (Minutes 1)
    metric =
      Metric "metric" $
        Labels
          [ Selector "foo" MatchEquals "bar",
            Selector "bazz" MatchNotEquals "buzz"
          ]
