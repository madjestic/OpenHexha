{-# LANGUAGE OverloadedStrings #-}

module Net.CoinbasePro.WebSocketFeed.Channel.Status
    ( Status (..)
    ) where

import           Data.Aeson                   (FromJSON, parseJSON, withObject,
                                               (.:))

import           Net.CoinbasePro.MarketData.Types (Product)
import           Net.CoinbasePro.Types            (Currency)


data Status = Status
    { currencies :: [Currency]
    , products   :: [Product]
    } deriving (Eq, Show)


instance FromJSON Status where
    parseJSON = withObject "status" $ \o -> Status
        <$> o .: "currencies"
        <*> o .: "products"
