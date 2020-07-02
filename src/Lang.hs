{-# LANGUAGE OverloadedStrings #-}
module Lang
        ()
where

import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )
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
        show (VecFn fn) = undefined

-- Note: Not including larger ranges for simplicty/testing difficulties.
data TimeRange = Seconds Int | Minutes Int

instance Show TimeRange where
        show (Seconds x) = show x ++ "s"
        show (Minutes x) = show x ++ "m"

data RangeVector = MetricRange Metric TimeRange | SubQuery InstantVector TimeRange TimeRange

instance Show RangeVector where
        show x = case x of
                MetricRange m t -> concat [show m, "[", show t, "]"]
                SubQuery vec x y ->
                        concat [show x, "[", show x, ":", show y, "]"]

data Function = Vector Scalar | Round InstantVector (Maybe Int) | Sqrt InstantVector | Floor InstantVector | Ceil InstantVector | Rate RangeVector | Ln InstantVector | Increase RangeVector

instance Show Function where
        show x = concat [fn, "(", innards, ")"]
            where
                (fn, innards) = case x of
                        Vector x -> ("vector", show x)
                        Round x y ->
                                ("round", concat [show x, show (fromMaybe 1 y)])
                        Sqrt     v -> ("sqrt", show v)
                        Floor    v -> ("floor", show v)
                        Ceil     v -> ("ceil", show v)
                        Rate     r -> ("rate", show r)
                        Ln       v -> ("ln", show v)
                        Increase r -> ("increase", show r)

data AST = ASTScalar Scalar | InstantVector InstantVector | RangeVector RangeVector

instance Show AST where
  show x = case x of
    ASTScalar v -> show v
    InstantVector v -> show v
    RangeVector v -> show v
