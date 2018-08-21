{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Methods.Player where

import KodiRPC.Calls
import KodiRPC.Types

import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Aeson.Types

getActivePlayers = Method 1.0 2.0 "Player.GetActivePlayer" empty
getItem          = Method 1.0 2.0 "Player.GetItem" empty
