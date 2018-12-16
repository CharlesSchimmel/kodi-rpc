{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Methods.Application where

import KodiRPC.Types

import Data.Aeson
import Data.Aeson.Types
import Prelude as P
import qualified Data.Text as T
import Data.HashMap.Strict as HM
import qualified Data.Vector as V

getProperties props = method' "Application.GetProperties" $ fromList [("properties", propStrings)]
  where propStrings = Array . V.fromList $ P.map showApplicationProperty props

data ApplicationProperty = Volume | Muted | Name | Version
            deriving (Show)

showApplicationProperty :: ApplicationProperty -> Value
showApplicationProperty = String . T.toLower . T.pack . show
