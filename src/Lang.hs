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

data AggOp = Avg | Count | CountValues | Max | Min  | Stddev | Stdvar | Sum | TwoArityAggOp TwoArityAggOp

instance Show AggOp where
        show x = case x of
                Avg                    -> "avg"
                Count                  -> "count"
                CountValues            -> "count_values"
                Max                    -> "max"
                Min                    -> "min"
                Stddev                 -> "stdev"
                Stdvar                 -> "stdvar"
                Sum                    -> "sum"
                TwoArityAggOp agg -> show agg

data TwoArityAggOp = Bottomk Int | Quantile Float | Topk Int

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


instance Show TwoArityAggOp where
        show x = case x of
                Bottomk  _ -> "bottomk"
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

data InstantVector = MetricVec Metric | Scalar Int

instance Show InstantVector where
  show (Scalar v) = show v
  show (MetricVec m) = show m


-- Note: Not including larger ranges for simplicty/testing difficulties.
data TimeRange = Seconds Int | Minutes Int

instance Show TimeRange where
        show (Seconds x) = show x ++ "s"
        show (Minutes x) = show x ++ "m"

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
