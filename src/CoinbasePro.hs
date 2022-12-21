module CoinbasePro
  ( Ticker (..)
  , TickerCommand (..)
  , graphQueryString
  , graphLogString
  , logPrice
  , readPrice
  ) where

import qualified System.IO.Streams    as Streams
import System.IO.Strict               as S
import Data.Time.Clock
import qualified Data.List            as DL
import Data.Text.Chart                as C
import Data.Text   (pack)
import Control.Concurrent.MVar
import Control.Monad (forever)
import System.IO

import           Net.CoinbasePro.WebSocketFeed         (subscribeToFeed)
import           Net.CoinbasePro.WebSocketFeed.Request (ChannelName (..))
import           Net.CoinbasePro.WebSocketFeed.Channel (ChannelMessage (..))
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Match    as M
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Open     as O
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Done     as D
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Received as R
import           Net.CoinbasePro.Unauthenticated
import           Net.CoinbasePro.Request
import           Net.CoinbasePro.Environment
import           Net.CoinbasePro.Types

data Ticker =
  Ticker
  { command :: MVar TickerCommand
  , msg     :: String }

newtype TickerCommand = Message String deriving Show

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- String Grapher

options' :: C.Options
options' =
  C.Options { height = 14 }

graphLogString :: IO String
graphLogString = do
  s <- S.readFile logFile
  let d = DL.reverse . take 80 . DL.reverse $ fmap round (read <$> lines s :: [Double]) :: [Integer]
      result = unlines $ plotWithString options' d
  return result

graphQueryString' :: IO String
graphQueryString' = do
  let d = candles (ProductId $ pack "BTC-USD") (Just fromDate) (Just toDate) Day
  cs <- run Sandbox d
  let ps = (round  . unPrice . low <$> cs) :: [Integer]
      result = unlines $ plotWithString options' ps
  return result

graphQueryString :: TickerCommand -> IO String
graphQueryString (Message s) = do
  let d = candles (ProductId $ pack s) (Just fromDate) (Just toDate) Day
  cs <- run Sandbox d
  let ps = (round  . unPrice . low <$> cs) :: [Integer]
      result = unlines $ plotWithString options' ps
  return result
  
fromDate :: UTCTime
fromDate = read "2021-01-01 00:00:00 UTC"

toDate :: UTCTime
toDate =   read "2021-03-01 00:00:00 UTC"

---------------------------------------------------------------------------------------------------------------------------------------------------------------
--- COINBASE PRO ----------------------------------------------------------------------------------------------------------------------------------------------

logFile :: FilePath
logFile = ".log" :: FilePath

logPrice :: Maybe String -> IO String
logPrice Nothing  = do
  S.readFile logFile
logPrice (Just s) = do
  h <- openFile logFile AppendMode
  hPutStrLn h s
  hClose h

  S.readFile logFile

readPrice :: Maybe ChannelMessage -> IO (Maybe String)
readPrice m =
  case m of
    Just (MatchMessage x) -> return $ Just (show $ unPrice $ M.price x)
    Just (OpenMessage  x) -> return $ Just (show $ unPrice $ O.price x)
    Just (ReceivedMessage r) -> return $ rPrice r
    Just (DoneMessage     d) -> return $ dPrice d
    _ -> return Nothing
    where
      dPrice d =
        case D.price d of
          Just p -> Just (show $ unPrice p)
          Nothing -> Nothing

      rPrice r =
        case R.price r of
          Just p -> Just (show $ unPrice p)
          Nothing -> Nothing

logPriceCB :: IO ()
logPriceCB = do
  msgs <- subscribeToFeed [ProductId (pack "BTC-USD")] [Full] Sandbox Nothing
  forever $ Streams.read msgs >>= readPrice >>= logPrice
