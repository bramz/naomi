module Main where

import Control.Concurrent.STM (newTChanIO)
import Wuss (runSecureClient)

import Naomi.Core (client)

main :: IO ()
main = do
    chan <- newTChanIO
    runSecureClient "gateway.discord.gg" 443 "/" $ client chan