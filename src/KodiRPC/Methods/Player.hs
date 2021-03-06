{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Methods.Player where

import KodiRPC.Calls
import KodiRPC.Types.Base
import KodiRPC.Types.Fields.All

import Prelude as P
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict as HM
import Text.Regex
import qualified Data.Text as T
import qualified Data.Vector as V

getActivePlayers           = method' "Player.GetActivePlayers" HM.empty

getItem :: Int -> [AllField] -> Method
getItem playerid fields    = method' "Player.GetItem" propMap
  where propMap = fromList [("playerid", toJSON playerid), ("properties", propNames)]
        propNames = Array . V.fromList $ P.map (String . showAllField) fields

open                       = method' "Player.Open"
getProperties :: Int -> [PropertyName] -> Method
getProperties playerid propNames = method' "Player.GetProperties" propMap
  where propMap = fromList [("playerid", toJSON playerid),("properties", propNames')]
        propNames' = Array . V.fromList $ P.map showPropertyName propNames

-- Could create new type for kodi-specific paths (plugin, video, music, etc.)
openPath :: T.Text -> Method
openPath path = open item
  where item   = HM.singleton (T.pack "item") pathHM
        pathHM = Object $ HM.singleton (T.pack "file") (String path)

openYoutube :: T.Text -> Method
openYoutube = openPath . asYTPluginPath

asYTPluginPath link = T.append "plugin://plugin.video.youtube/?action=play_video&videoid=" link

matchYouTubeId :: T.Text -> Maybe T.Text
matchYouTubeId link = T.pack . last <$> matchRegex (mkRegex youtubeIdRegex) (T.unpack link)
  where youtubeIdRegex = "(youtu\\.be\\/|youtube\\.com\\/(watch\\?(.*&)?v=|(embed|v)\\/))([^\\?&\"'>]+)" :: String

data PropertyName = Audiostreams | Canchangespeed  | Canmove            | Canrepeat       | Canrotate
                                 | Canseek         | Canshuffle         | Canzoom         | Currentaudiostream
                                 | Currentsubtitle | Currentvideostream | Live            | Partymode
                                 | Percentage      | Playlistid         | Position        | Repeat
                                 | Shuffled        | Speed              | Subtitleenabled | Subtitles
                                 | Time            | Totaltime          | Type            | Videostreams
                                deriving (Show)

showPropertyName :: PropertyName -> Value
showPropertyName = String . T.toLower . T.pack . show
