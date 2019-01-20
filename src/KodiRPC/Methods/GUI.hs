{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module KodiRPC.Methods.GUI where

import KodiRPC.Types.Base

import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Text as T
import GHC.Generics


getProperties :: [GUIProp] -> Method
getProperties prop = method' "GUI.GetProperties" $ HM.singleton "properties" (toJSON prop)

data GUIProp = Currentwindow
             | Currentcontrol
             | Skin
             | Fullscreen
             | Stereoscopicmode
          deriving (Show, Generic, Enum, Bounded, Read)

instance ToJSON GUIProp where
    toJSON = String . T.toLower . pack . show

