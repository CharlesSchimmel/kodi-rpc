{-# LANGUAGE OverloadedStrings #-}
module Player where

import KodiMethods
import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Aeson.Types

getActivePlayers = Method 1.0 2.0 "Player.GetActivePlayer" Null
getItem = Method 1.0 2.0 "Player.GetItem" Null
-- getPlayers :: Maybe String -> Method
-- getPlayers media = Method 1.0 2.0 "Player.GetPlayer" $ maybe Null (\x -> Object . HM.singleton String "media" $ String x) media
