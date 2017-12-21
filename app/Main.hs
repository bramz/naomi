module Main where

import Control.Logging (withStdoutLogging)
import Control.Concurrent.STM (newTChanIO)
import Wuss (runSecureClient)

import Naomi.Core (client)

main :: IO ()
main = withStdoutLogging $ do
    chan <- newTChanIO
    runSecureClient "gateway.discord.gg" 443 "/" $ client chan