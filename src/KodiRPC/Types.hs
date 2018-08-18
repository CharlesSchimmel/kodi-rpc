{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KodiRPC.Types where

import Prelude as P
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Default.Class
import Data.Either
import Data.Monoid
import Data.Text as T
import Data.Text.Read
import Data.Char as C (toUpper)
import Data.Map.Strict as HM
import GHC.Generics
import Lens.Micro.Platform hiding ((.=))
import Network.HTTP.Req as R

type Params = Value

data KodiInstance = KodiInstance
   { _server :: T.Text
   , _port   :: Int
   } deriving (Generic, Show)

-- Basic components of a RPC Method
data Method = Method 
   { _methodId      :: Double
   , _methodJsonrpc :: Double
   , _methodStr     :: String
   , _methodParams  :: Params
   } deriving (Generic, Show)

instance ToJSON Method where
   toJSON (Method id jsonrpc method params) = object [
               "id"      .= show id
             , "jsonrpc" .= show jsonrpc
             , "method"  .= method
             , "params"  .= toJSON params
             ]

data Notif = Notif
   { _notifJsonrpc :: String
   , _notifMethod  :: String
   , _notifParams  :: Object
   } deriving (Generic, Show)

instance FromJSON Notif where
   parseJSON = withObject "Notif" $ \v -> Notif
      <$> v.: "jsonrpc"
      <*> v.: "method"
      <*> v.: "params"

data GUIProp = Currentwindow
             | Currentcontrol
             | Skin
             | Fullscreen
             | Stereoscopicmode
          deriving (Show, Generic, Enum, Bounded, Read)

instance ToJSON GUIProp where
    toJSON = String . T.toLower . pack . show

showGUIProp :: GUIProp -> Value
showGUIProp  = String . toLower . pack . show

-- readGUIProp :: Value -> GUIProp
-- readGUIProp g = read prop :: GUIProp
--     where prop = (C.toUpper x) : xs
--           x = T.head g
--           xs = unpack $ T.tail g

makeLenses ''KodiInstance
makeLenses ''Method
makeLenses ''Notif
