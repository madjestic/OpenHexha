{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import qualified Brick.BChan as BC       
import qualified Brick.Main  as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border       as B
import Brick.Widgets.List             as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap        as A
import qualified Data.Vector          as Vec
import Brick.Types ( Widget )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  )
import Brick.Util       (on)
import Control.Concurrent

import CoinbasePro
-- import IEX

-- import Debug.Trace    as DT

data AppState =
     AppState
     {
       _header  :: String
     , _appList :: L.List () String
     }
makeLenses ''AppState

-- CoinbasePro Test:
-- testCBP :: IO ()
-- testCBP = do
--   msgs <- subscribeToFeed [ProductId (pack "BTC-USD")] [Full] Sandbox Nothing
--   forever $ Streams.read msgs >>= readPrice >>= logPrice >> graphLogString >>= (\s ->  M.defaultMain theApp initialState)

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Brick Logic

drawUI :: AppState -> [Widget ()]
drawUI (AppState s l) = [ui]
    where
        label = current
        current
          = case l^.L.listSelectedL of
              Nothing -> str "-"
              Just i -> str $ (listElements l)Vec.!i

        graph
          = B.borderWithLabel label $
            hLimit 80 $
            vLimit 15 $
            str s

        box
          = B.borderWithLabel label $
            hLimit 69 $
            vLimit 15 $
            L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [
                                C.hCenter graph
                              , C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

ticker :: BC.BChan Ticker -> IO ()
ticker chan = do
  t  <- BC.readBChan chan
  BC.writeBChan chan t
  msg' <- graphQueryString =<< readMVar (command t)
  threadDelay 1000000
  BC.writeBChan chan $ Ticker (command t) msg'

appEvent :: T.BrickEvent () Ticker -> T.EventM () AppState ()
appEvent (T.AppEvent (Ticker cmd m)) = do
  l <- use appList
  let cs = fromMaybe (0, "BTC-USD") (listSelectedElement l)
  _ <- liftIO $ swapMVar cmd (Message (snd cs))
  header .= m
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] -> do
          l <- use appList
          let
            el  = nextElement (L.listElements l)
            pos = Vec.length $ l^.L.listElementsL
          appList .= L.listInsert pos el l

        V.EvKey (V.KChar '-') [] -> do
          l <- use appList
          case l^.L.listSelectedL of
            Nothing -> appList .= l
            Just i  -> appList .= L.listRemove i l

        V.EvKey V.KUp [] -> do
          l <- use appList
          appList .= listMoveUp l

        V.EvKey V.KDown [] -> do
          l <- use appList
          appList .= listMoveDown l
          
        V.EvKey V.KEsc [] -> M.halt

        _ -> return ()

    where
      nextElement :: Vec.Vector String -> String
      nextElement v =
        fromMaybe "?" $ Vec.find (flip Vec.notElem v)
        (Vec.fromList
          [ -- supported assets
            "BTC-USD"
          , "BTC-EUR"
          , "ETH-USD"
          , "LTC-USD"
          ]
        )
        
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = str s -- if sel
                   -- then withAttr customAttr (str $ "<" <> s <> ">")
                   -- else str s
    in C.hCenter $ str "Item " <+> selStr (show a)

-- customAttr :: A.AttrName
-- customAttr = L.listSelectedAttr <> "custom"

sysfg :: V.Color
sysfg = V.rgbColor (00::Integer) (99::Integer) (00::Integer)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.green `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.green)
    --, (customAttr,            fg V.red)
    ]

initialState :: AppState
initialState =
  AppState
  { _header = "initialState"
  , _appList = applist
  }
  where
    applist =
      L.list () (Vec.fromList
                  [ "BTC-USD"
                  , "BTC-EUR"
                  ]) 1

theApp :: M.App AppState Ticker ()
theApp =
    M.App { M.appDraw         = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent  = appEvent
          , M.appStartEvent   = return ()
          , M.appAttrMap      = const theMap
          }

main :: IO ()
main = do
  eventChan  <- BC.newBChan 10
  _ <- forkIO $ do
    cmd <- newEmptyMVar
    putMVar cmd (Message "BTC-USD")
    BC.writeBChan eventChan $ Ticker cmd "Loading data..."
    forever $ ticker eventChan
  
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  let runState' = M.customMain initialVty buildVty
                 (Just eventChan) theApp initialState
  void runState'
