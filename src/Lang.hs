module Lang
        ()
where

-- | MatchType is a matcher
data MatchType = MatchEquals | MatchNotEquals | MatchRegexp | MatchNotRegexp

-- | Selector is selection
data Selector = Selector String MatchType String

data Operator = Add | Div | Eql | EqlRegex | Gte | Gtr | And | Or | Less | Lte | LUnless | Mod | Mul | Neq | NeqRegex | Pow | Sub

data Aggregator = Avg | Bottomk Int | Count | CountValues | Max | Min | Quantile Float | Stddev | Stdvar | Sum | Topk Int

type Scalar = Either String Float

data Metric = Metric String [Selector]

data InstantVector = Scalar Scalar | Operator Operator InstantVector InstantVector | Aggregator Aggregator InstantVector | VecFn Function

data Function = Vector Scalar | Round InstantVector (Maybe Int) | Sqrt InstantVector | Floor InstantVector | Ceil InstantVector | Rate RangeVector | Ln InstantVector | Increase RangeVector

data RangeVector = MetricRange Metric Int Int | SubQuery InstantVector Int Int

data Expr = InstantVector | RangeVector
