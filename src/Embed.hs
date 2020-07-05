{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Embed
        ()
where

import           Lang
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Aeson                    as A
import Data.Aeson (ToJSON, toJSON, (.=))

embed :: InstantVector -> InstantVector
embed v = v

queryLabel = "__cortex_queries__"

embeddedMetric = "__embedded_queries__"

newtype Encoded = Encoded {decode :: [InstantVector]}

instance ToJSON Encoded where
  toJSON x = A.object [ queryLabel .= queries ]
    where queries = map show $ decode  x

instance Show Encoded where
        show = CL.unpack . A.encode

x = Encoded [qry]
