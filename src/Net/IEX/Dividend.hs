{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Dividend (Dividend(..)) where

-- import Data.Maybe
import Data.Aeson
import GHC.Generics

data Dividend = Dividend {
  exDate :: String,
  paymentDate :: String,
  recordDate :: String,
  declaredDate :: String,
  amount :: Double,
  flag :: String,
  dtype :: String,
  qualified :: String,
  indicated :: String
} deriving (Generic, Show, Eq)

customOptionsDividend :: Options
customOptionsDividend =
  defaultOptions {
    fieldLabelModifier = let f "dtype" = "type"
                             f other = other
                         in f
    }

instance ToJSON Dividend
instance FromJSON Dividend where
  parseJSON = genericParseJSON customOptionsDividend
