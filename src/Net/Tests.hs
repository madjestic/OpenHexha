module Tests where

import Data.Aeson
import Network.HTTP.Conduit
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as L8

import Net.Stocks
import qualified Net.IEX.TimeSeries         as IEXTimeSeries

import Debug.Trace    as DT

getNonJSONData :: String -> IO (Either SomeException L8.ByteString)
getNonJSONData query = try $ simpleHttp query

type AuthAndSymbol = (String, Symbol)
type Symbol = String

test' :: IO (Maybe [IEXTimeSeries.TimeSeries])
test' =
  do
    obj <- getNonJSONData "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_your_token_id"
    putStrLn $ show obj
    -- case (DT.trace ("obj :" ++ show obj) $ obj) of
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

getTS'' :: AuthAndSymbol -> QSParms -> IO (Maybe [IEXTimeSeries.TimeSeries])
-- getTS'' (auth, symb) parms = do
getTS'' _ _ = do  
  obj <- getNonJSONData "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_your_token_id"
         -- baseURL
         -- ++ "/time-series/REPORTED_FINANCIALS/"
         -- ++ symb
         -- ++ "/10-Q"
         -- ++ "?"
         -- ++ "from=2018-01-01&to=2019-06-01"
         -- ++ tokenize' auth
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr


parms = QSParms 0 0
symb  = "AAPL"
token = "Tpk_your_token_id"
query = "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_your_token_id"

test =
  do
    x <- getTS'' (token, symb) parms
    print $ length x
