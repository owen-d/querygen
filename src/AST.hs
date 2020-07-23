{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AST where

import Data.List (intercalate)
import Lang

class BinaryOp a b c | a b -> c where
  binOp :: b -> a -> a -> c

instance BinaryOp InstantVector Operator InstantVector where
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

qry :: PromQL
qry = Aggregation $ Aggregator Sum groupBy rate
  where
    groupBy = Grouping {groups = ["bazz", "borg"], without = False}
    rate = Function $ Rate $ MetricRange metric (Minutes 1)
    metric =
      Metric "metric" $
        Matchers
          [ Selector "foo" MatchEquals "bar",
            Selector "bazz" MatchNotEquals "buzz"
          ]
