{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Net.CoinbasePro.WebSocketFeed.Channel.Ticker
    ( Ticker (..)
    ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Time.Clock   (UTCTime)

import           Net.CoinbasePro.Types (Price, ProductId, Sequence, Side, Size)


data Ticker = Ticker
    { tradeId   :: Maybe Int
    , sequence  :: Sequence
    , time      :: Maybe UTCTime
    , productId :: ProductId
    , price     :: Price
    , side      :: Maybe Side
    , lastSize  :: Maybe Size
    , bestBid   :: Price
    , bestAsk   :: Price
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Ticker
