{-# LANGUAGE OverloadedStrings #-}
module Lang where

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

instance HasLabels Selector where
  labels x = Labels [x]

data Labels = Labels [Selector]

instance HasLabels Labels where
  labels x = x

instance Semigroup Labels where
  (Labels xs) <> (Labels ys) = Labels (xs ++ ys)

instance Monoid Labels where
  mempty = Labels []

instance Show Labels where
  show (Labels []) = "{}"
  show (Labels xs) = "{" ++ ls ++ "}"
    where ls = intercalate ", " $ map show xs

class HasLabels a where
  labels :: a -> Labels

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

data Metric = Metric String Labels

instance Show Metric where
  show (Metric x ls) = x ++ (show ls)

data Grouping = Grouping { groups :: [String]
                         , without :: Bool
                         }

instance Show Grouping where
        show (Grouping [] False) = " by ()"
        show (Grouping xs False) = " by(" ++ (intercalate ", " xs) ++ ")"
        show (Grouping [] True ) = []
        show (Grouping xs True ) = " without(" ++ (intercalate ", " xs) ++ ")"

data InstantVector = Operator Operator InstantVector InstantVector | Aggregator Aggregator Grouping InstantVector | VecFn Function | Scalar Int

instance Show InstantVector where
        show (Operator op vec1 vec2) =
                intercalate " " [show vec1, show op, show vec2]
        show (Aggregator agg grp vec) = case agg of
                TwoArityAggregator x -> concat
                        [show x, show grp, " (", v, ",", show vec, ")"]
                    where
                        v = case x of
                                Bottomk  v -> show v
                                Topk     v -> show v
                                Quantile v -> show v
                y -> concat [show y, show grp, " (", show vec, ")"]
        show (VecFn  fn) = show fn
        show (Scalar v ) = show v

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

data Function = Vector Int | Round InstantVector (Maybe Int) | Sqrt InstantVector | Floor InstantVector | Ceil InstantVector | Rate RangeVector | Ln InstantVector | Increase RangeVector

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

data AST = ASTScalar Int | InstantVector InstantVector | RangeVector RangeVector

instance Show AST where
        show x = case x of
                ASTScalar     v -> show v
                InstantVector v -> show v
                RangeVector   v -> show v


qry = Aggregator Sum groupBy $ Operator Mul (VecFn $ rate) (Scalar 2)
    where
        rate   = Rate $ MetricRange metric (Minutes 1)
        metric = Metric
                "metric" $ Labels
                [ Selector "foo"  MatchEquals    "bar"
                , Selector "bazz" MatchNotEquals "buzz"
                ]
        groupBy = Grouping { groups = ["bazz", "borg"], without = False }
