{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Net.CoinbasePro.Unauthenticated
   ( products
   , aggregateOrderBook
   , fullOrderBook
   , trades
   , candles
   , stats
   , currencies
   , time
   ) where

import           Data.Time.Clock                           (UTCTime)
import           Servant.Client                            (ClientM)

import           Net.CoinbasePro.Headers                       (userAgent)
import           Net.CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           Net.CoinbasePro.MarketData.FullOrderBook      (FullOrderBook)
import           Net.CoinbasePro.MarketData.Types              (AggregateBookLevel (..),
                                                                CBTime,
                                                                FullBookLevel (..),
                                                                Product, Trade)
import           Net.CoinbasePro.Types                         (Candle,
                                                                CandleGranularity,
                                                                Currency, ProductId,
                                                                TwentyFourHourStats)
import qualified Net.CoinbasePro.Unauthenticated.API           as API


-- | https://docs.pro.coinbase.com/#get-products
products :: ClientM [Product]
products = API.products userAgent


-- | https://docs.pro.coinbase.com/#get-product-order-book
aggregateOrderBook :: ProductId -> Maybe AggregateBookLevel -> ClientM AggregateOrderBook
aggregateOrderBook prid agg = API.aggregateOrderBook prid agg userAgent


-- | https://docs.pro.coinbase.com/#get-product-order-book
fullOrderBook :: ProductId -> ClientM FullOrderBook
fullOrderBook prid = API.fullOrderBook prid (Just FullBookLevel) userAgent


-- | https://docs.pro.coinbase.com/#get-trades
trades :: ProductId -> ClientM [Trade]
trades prid = API.trades prid userAgent


-- | https://docs.pro.coinbase.com/#get-historic-rates
-- https://api.pro.coinbase.com/products/BTC-USD/candles?start=2018-07-10T12:00:00&stop=2018-07-15T12:00:00&granularity=900
candles :: ProductId -> Maybe UTCTime -> Maybe UTCTime -> CandleGranularity -> ClientM [Candle]
candles prid start end cg = API.candles prid start end cg userAgent


-- | https://docs.pro.coinbase.com/#get-24hr-stats
stats :: ProductId -> ClientM TwentyFourHourStats
stats prid = API.stats prid userAgent


-- | https://docs.pro.coinbase.com/#get-currencies
currencies :: ClientM [Currency]
currencies = API.currencies userAgent


-- | https://docs.pro.coinbase.com/#time
time :: ClientM CBTime
time = API.time userAgent
