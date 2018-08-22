{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module KodiRPC.Methods.GUI where

import KodiRPC.Types

import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Text
import GHC.Generics


getProperties :: [GUIProp] -> Method
getProperties prop = method' "GUI.GetProperties" $ HM.singleton "properties" (toJSON prop)
