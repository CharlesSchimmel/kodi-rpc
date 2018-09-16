{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Methods.Player where

import KodiRPC.Calls
import KodiRPC.Types

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict as HM
import Text.Regex
import qualified Data.Text as T

getActivePlayers = method' "Player.GetActivePlayer" HM.empty
getItem          = method' "Player.GetItem" HM.empty
open             = method' "Player.Open"

-- Could create new type for kodi-specific paths (plugin, video, music, etc.)
openPath :: T.Text -> Method
openPath path = open item
  where item   = HM.singleton (T.pack "item") pathHM
        pathHM = Object $ HM.singleton (T.pack "file") (String path)

openYoutube :: T.Text -> Method
openYoutube link = openPath $ T.append "plugin://plugin.video.youtube/?action=play_video&videoid=" link

matchYouTubeId :: T.Text -> Maybe T.Text
matchYouTubeId link = T.pack . last <$> matchRegex (mkRegex youtubeIdRegex) (T.unpack link)
  where youtubeIdRegex = "(youtu\\.be\\/|youtube\\.com\\/(watch\\?(.*&)?v=|(embed|v)\\/))([^\\?&\"'>]+)" :: String
