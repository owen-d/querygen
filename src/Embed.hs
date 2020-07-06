{-# LANGUAGE OverloadedStrings #-}

module Embed where

import           Lang
import qualified Data.ByteString.Lazy.Char8    as CL
import qualified Data.Aeson                    as A
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , (.=)
                                                )

embed :: [InstantVector] -> InstantVector
embed = MetricVec . (Metric embeddedMetric) . labels . Encoded

queryLabel :: String
queryLabel = "__cortex_queries__"

embeddedMetric :: String
embeddedMetric = "__embedded_queries__"

newtype Encoded = Encoded {decode :: [InstantVector]}

instance HasLabels Encoded where
        labels x = Labels $ [Selector queryLabel MatchEquals queries]
                where queries = (CL.unpack . A.encode) x


instance ToJSON Encoded where
        toJSON x = A.object ["Concat" .= queries]
                where queries = map show $ decode x

instance Show Encoded where
        show (Encoded xs) = show $ embed xs

tst :: Encoded
tst = Encoded [qry]
