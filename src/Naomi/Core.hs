{-# LANGUAGE OverloadedStrings #-}
module Naomi.Core (client) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TChan, newTChan, writeTChan, readTChan)
import Control.Monad (forever, void)
import qualified Control.Logging as L (debug, log)
import Data.Text (pack)
import Network.WebSockets (ClientApp, receiveData, sendTextData)

import Naomi.Discord.Packet (Packet)

client :: TChan Packet -> ClientApp ()
client chan conn = do
    L.log "Connected to Discord"

    void . forkIO . forever $ do
        msg <- receiveData conn
        L.debug $ pack $ show msg
        atomically $ writeTChan chan msg

    getChar >> return ()
