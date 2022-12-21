module IEX
  (
  ) where

import Data.Aeson hiding ((.=))
import Data.Aeson.Key
import Data.Aeson.Types (parse)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Utils
fromJust :: Monoid a => Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = mempty

---------------------------------------------------------------------------------------------------------------------------------------------------------------
--- IEX CLOUD -------------------------------------------------------------------------------------------------------------------------------------------------

type AuthAndSymbol = (String, Symbol)
type Symbol = String

getNonJSONData :: String -> IO (Either SomeException L8.ByteString)
getNonJSONData query' = try $ simpleHttp query'

symb :: String
symb  = "AAPL"
token :: String
token = "Tpk_your_token_id"
query :: String
query = "https://sandbox.iexapis.com/stable/stock/aapl/quote?token=Tpk_your_token_id"
        -- "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2010-01-01&interval=2&format=csv?token=Tpk_your_token_id"
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/previous/quote?token=Tpk_your_token_id" -- previous day range
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/3m?token=Tpk_your_token_id"          -- 3 months range
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/1m?token=Tpk_your_token_id"          -- 1 month  range
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/7d?token=Tpk_your_token_id"          -- 7 days
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/1d?token=Tpk_your_token_id"          -- 1 day per minute
        -- https://iexcloud.io/docs/api/#historical-prices

        -- "https://sandbox.iexapis.com/stable/stock/AAPL/intraday-prices?token=Tpk_your_token_id"   -- intraday prices
        -- https://iexcloud.io/docs/api/#intraday-prices -- API docs.
-- TODO: historic prices rest call   type
-- TODO: hustoric prices rest return type
-- TODO: same for intraday prices: intraday type call
--                                 intraday type return

testIEX :: IO ()
testIEX = do
  obj <- getNonJSONData "https://sandbox.iexapis.com/stable/stock/AAPL/quote?token=Tpk_your_token_id"
  --let obj'        = fromJust ( fromMaybe mempty $ decode ((\(Right x) -> x) obj) :: Maybe Object)
  let obj'        = fromJust ( fromMaybe mempty $ decode (case obj of
                                                            (Right x) -> x
                                                            (Left  _) -> L8.pack ""))
      latestPrice = parse (obj' .:) (fromString "latestPrice") :: Result Double
      symbol      = parse (obj' .:) (fromString "symbol")      :: Result String

  let symbol' =
        case symbol of
          Success x -> x
          Error   e -> e
  let latestPrice' =
        case latestPrice of
          Success x -> x
          Error   _ -> (-1)

  putStrLn $ symbol' ++ " : " ++ show latestPrice'
  return ()

-- TODO : do similar to CoinbasePro logPrice
logPriceIEX :: IO ()
logPriceIEX = undefined

