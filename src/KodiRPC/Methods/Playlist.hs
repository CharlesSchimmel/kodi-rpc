{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Methods.Playlist where

import KodiRPC.Types.Base
import KodiRPC.Types.Fields.All as All

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

getItems :: Int -> [AllField] -> Method
getItems id props = method' "Playlist.GetItems" propMap
  where propMap = HM.fromList [("playlistid", toJSON id), ("properties", props')]
        props' = Array . V.fromList . map (String . showAllField) $ props

basePlaylistFields = [ All.Displayartist
                     , All.Album
                     , All.Title
                     , All.Season
                     , All.Episode
                     ]

type Item = T.Text
type Id = Int

add :: Id -> Item -> Method
add id item = method' "Playlist.Add" propMap
  where propMap = HM.fromList [ ("playlistid", toJSON id), ("item", pathHM) ]
        item' = HM.singleton (T.pack "item") pathHM
        pathHM = Object $ HM.singleton (T.pack "file") (String  item)
