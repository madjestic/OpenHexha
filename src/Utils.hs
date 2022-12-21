module Utils
  ( getTimeSeries
  ) where

import Data.Aeson
import Network.HTTP.Conduit
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as L8
import Net.Stocks
import qualified Net.IEX.TimeSeries         as IEXTS

--import Debug.Trace    as DT

fromMaybe :: Maybe a -> [a]
fromMaybe x =
  case x of
    Just x'  -> [x']
    Nothing  -> []

getNonJSONData :: String -> IO (Either SomeException L8.ByteString)
getNonJSONData query' = try $ simpleHttp query'

type AuthAndSymbol = (String, Symbol)
type Symbol = String

-- write :: Project -> FilePath -> IO ()
-- write prj fileOut =
--   B.writeFile fileOut $ encodePretty' config prj
--   where
--     config = defConfig { confCompare = comp }


test' :: IO (Maybe [IEXTS.TimeSeries])
test' =
  do
    -- obj <- getNonJSONData "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_your_token_id"
    obj <- getNonJSONData "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?last=1&token=Tpk_your_token_id"
    -- putStrLn $ show obj
    -- case (DT.trace ("obj :" ++ show obj) $ obj) of
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

getTimeSeries :: AuthAndSymbol -> QSParms -> IO (Maybe [IEXTS.TimeSeries])
-- getTS'' (auth, symb) parms = do
getTimeSeries _ _ = do  
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


parms :: QSParms
parms = QSParms 0 0
symb :: String
symb  = "AAPL"
token :: String
token = "Tpk_your_token_id"
query :: String
query = "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_your_token_id"

main :: IO ()
main = do
  -- x <- getTimeSeries (token, symb) parms
  x <- test'
  print $ length x

-- obj = L8.pack "{\"name\":\"Dave\",\"age\":2}
-- decode obj :: Maybe Object
-- Just (fromList [("name",String "Dave"),("age",Number 2.0)])
-- obj = decode (L8.pack "{\"name\":\"Dave\",\"age\":2}") :: Maybe Object
-- (.:) (Prelude.head obj) (Data.Text.pack "age")
-- parse ((.:) (Prelude.head obj))(Data.Text.pack "name") :: Result String
-- Success "Dave"

-- obj <- getNonJSONData "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?last=1&token=Tpk_your_token_id"
-- obj' = (\(Right x) -> x) obj
-- obj'' = decode obj' :: Maybe [Object]
-- parse ((.:) (Prelude.head $ fromMaybe obj'')) (Data.Text.pack "key")

-- obj'' = Prelude.head $ Prelude.head $ fromMaybe (decode obj'::Maybe [Object])
-- parse ((.:) obj'') (Data.Text.pack "key") :: Result String

-- https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?on=2020-01-01&token=Tpk_your_token_id
-- https://sandbox.iexapis.com/stable/stock/twtr/quote?token=Tpk_your_token_id
-- curl --header 'Accept: text/event-stream' https://sandbox-sse.iexapis.com/stable/forex1Second\?symbols\=USDCAD\&token\=Tpk_your_token_id
-- https://hackage.haskell.org/package/curl-1.3.8/docs/Network-Curl.html
