module Naomi.Core (client) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TChan, newTChan, writeTChan, readTChan)
import Control.Monad (forever, void)
import Network.WebSockets (ClientApp, receiveData, sendTextData)

import Naomi.Discord.Packet (Packet)

client :: TChan Packet -> ClientApp ()
client chan conn = do
    putStrLn "Connected!"

    void . forkIO . forever $ writeTChan chan <$> receiveData conn

    let loop = do packet <- atomically $ readTChan chan
                  print packet
                  loop
    loop
