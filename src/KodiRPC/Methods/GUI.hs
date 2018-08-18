{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GUI where

import Types
import Methods.Input
import Calls

import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Text
import GHC.Generics


getProperties :: [GUIProp] -> Method
getProperties prop = method' "GUI.GetProperties" (toJSON prop)


data GUIProp = Currentwindow
             | Currentcontrol
             | Skin
             | Fullscreen
             | Stereoscopicmode
          deriving (Generic)

instance ToJSON GUIProp where
            toJSON Currentwindow          = String "currentwindow"
            toJSON Currentcontrol         = String "currentcontrol"
            toJSON Skin                   = String "skin"
            toJSON Methods.GUI.Fullscreen = String "fullscreen"
            toJSON Stereoscopicmode       = String "stereoscopicmode"
