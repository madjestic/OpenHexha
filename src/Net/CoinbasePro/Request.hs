{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module Net.CoinbasePro.Request
    ( RequestPath
    , Body

    , CBGet
    , CBRequest

    , run
    , run_
    , runWithManager

    , Runner

    , emptyBody
    ) where

import           Control.Exception       (throw)
import           Control.Monad           (void)
import           Data.ByteString         (ByteString)
import           Data.Text               (unpack)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API             (Get, JSON, (:>))
import           Servant.Client

import           Net.CoinbasePro.Environment (Environment, apiEndpoint)
import           Net.CoinbasePro.Headers     (UserAgent, UserAgentHeader)


type CBGet a = UserAgentHeader :> Get '[JSON] a


type CBRequest a = UserAgent -> ClientM a

-- ^ Serialized as a part of building CBAccessSign
type RequestPath = ByteString

-- ^ Serialized as a part of building CBAccessSign
type Body        = ByteString


type Runner a = ClientM a -> IO a


------------------------------------------------------------------------------
-- | Runs a coinbase pro HTTPS request and returns the result `a`
--
-- > run Production products >>= print
--
run :: Environment -> ClientM a -> IO a
run env f = do
    mgr <- newManager tlsManagerSettings
    runWithManager mgr env f


------------------------------------------------------------------------------
-- | Same as 'run', except uses `()` instead of a type `a`
run_ :: Environment -> ClientM a -> IO ()
run_ = (void .) . run


------------------------------------------------------------------------------
-- | Allows the user to use their own 'Network.HTTP.Client.Types.ManagerSettings`
-- with 'run'
--
-- @
-- do $
-- mgr  <- newManager tlsManagerSettings
-- prds <- runWithManager mgr Production products
-- print prds
-- @
--
runWithManager :: Manager -> Environment -> ClientM a -> IO a
runWithManager mgr env f = either throw return =<<
    runClientM f (mkClientEnv mgr (BaseUrl Https api 443 mempty))
  where
    api = unpack $ apiEndpoint env


emptyBody :: ByteString
emptyBody = ""
