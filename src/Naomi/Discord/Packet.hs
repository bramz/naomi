{-# LANGUAGE OverloadedStrings #-}

module Naomi.Discord.Packet
    ( Opcode(..)
    , Packet(..)
    ) where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), (.:), object, withObject)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Scientific (floatingOrInteger)

data Packet
    = DispatchPacket Value Integer String
    | RegularPacket Opcode Value
    deriving (Show, Eq)

data Opcode
    = Heartbeat
    | Identify
    | StatusUpdate
    | VoiceStatusUpdate
    | VoiceServerPing
    | Resume
    | Reconnect
    | RequestGuildMembers
    | InvalidSession
    | Hello
    | HeartbeatACK
    deriving (Show, Eq, Ord, Enum, Bounded)

toOpcode :: Int -> Parser Opcode
toOpcode n = let n' = n - 1 in if n' < 0 || n' >= (fromEnum $ (maxBound :: Opcode))
                then fail "unknown opcode"
                else return $ [minBound..maxBound] !! n'

instance ToJSON Packet where
    toJSON (DispatchPacket d s t) = object ["op" .= (0 :: Int), "d" .= d, "s" .= s, "t" .= t]
    toJSON (RegularPacket op d)   = object ["op" .= (fromEnum op + 1), "d" .= d]

instance FromJSON Packet where
    parseJSON = withObject "Packet" $ \o -> do  n <- o .: "op"
                                                case floatingOrInteger n of
                                                    Left _   -> fail "opcode is not an integer"
                                                    Right n' -> if n' == 0
                                                                    then DispatchPacket <$> (o .: "d") <*> (o .: "s") <*> (o .: "t")
                                                                    else RegularPacket <$> toOpcode n' <*> (o .: "d")
