{-# LANGUAGE OverloadedStrings #-}
module Lang
        ()
where

import           Data.List                      ( intercalate )
-- | MatchType is a matcher
data MatchType = MatchEquals | MatchNotEquals | MatchRegexp | MatchNotRegexp

instance Show MatchType where
        show x = case x of
                MatchEquals    -> "="
                MatchNotEquals -> "!="
                MatchRegexp    -> "=~"
                MatchNotRegexp -> "!~"

-- | Selector is selection
data Selector = Selector String MatchType String

instance Show Selector where
        show (Selector label mt matcher) = concat
                [quote, label, quote, (show mt), quote, matcher, quote]
                where quote = "\""

data Operator = Add | Div | Eql | Gte | Gtr | And | Or | Less | Lte | Unless | Mod | Mul | Neq | Pow | Sub

instance Show Operator where
        show x = case x of
                Add    -> "+"
                Div    -> "/"
                Eql    -> "=="
                Gte    -> ">="
                Gtr    -> ">"
                And    -> "and"
                Or     -> "or"
                Less   -> "<"
                Lte    -> "<="
                Unless -> "unless"
                Mod    -> "%"
                Mul    -> "*"
                Neq    -> "!="
                Pow    -> "^"
                Sub    -> "-"

data Aggregator = Avg | Count | CountValues | Max | Min  | Stddev | Stdvar | Sum | TwoArityAggregator TwoArityAggregator

instance Show Aggregator where
        show x = case x of
                Avg                  -> "avg"
                Count                -> "count"
                CountValues          -> "count_values"
                Max                  -> "max"
                Min                  -> "min"
                Stddev               -> "stdev"
                Stdvar               -> "stdvar"
                Sum                  -> "sum"
                TwoArityAggregator x -> show x

data TwoArityAggregator = Bottomk Int | Quantile Float | Topk Int

instance Show TwoArityAggregator where
        show x = case x of
                Bottomk  v -> "bottomk"
                Topk     _ -> "topk"
                Quantile _ -> "quantile_over_time"

data Scalar = Scalar (Either String Float)

instance Show Scalar where
        show (Scalar x) = case x of
                Left  v -> v
                Right v -> show v

data Metric = Metric String [Selector]

instance Show Metric where
        show (Metric x []) = x
        show (Metric x ys) = concat [x, "{", labels, "}"]
                where labels = intercalate "," $ map show ys

data InstantVector = Operator Operator InstantVector InstantVector | Aggregator Aggregator InstantVector | VecFn Function

instance Show InstantVector where
        show (Operator op vec1 vec2) = concat [show vec1, show op, show vec2]
        show (Aggregator agg vec   ) = case agg of
                TwoArityAggregator x -> concat
                        [show x, "(", v, ",", show vec, ")"]
                    where
                        v = case x of
                                Bottomk  v -> show v
                                Topk     v -> show v
                                Quantile v -> show v
                y -> concat [show y, "(", show vec, ")"]
        show _ = ""

data Function = Vector Scalar | Round InstantVector (Maybe Int) | Sqrt InstantVector | Floor InstantVector | Ceil InstantVector | Rate RangeVector | Ln InstantVector | Increase RangeVector

data RangeVector = MetricRange Metric Int Int | SubQuery InstantVector Int Int

data Expr = InstantVector | RangeVector
