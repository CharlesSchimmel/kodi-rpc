{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module KodiRPC.Types where

import Prelude as P
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Types hiding (Result, Error)
import Data.Default.Class
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Text.Read
import Data.Traversable as Trv
import Data.Char as C (toUpper)
import Data.HashMap.Strict as HM
import GHC.Generics
import Lens.Micro.Platform hiding ((.=))
import Network.HTTP.Req as R

type Params = Object

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

method' :: String -> Params -> Method
method' = Method 1.0 2.0
methodNoP x = method' x HM.empty

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

data Response = Response
  { _result :: Either Value Value
  , _responseId :: String
  }
  deriving (Show)

instance FromJSON Response where
  parseJSON (Object v) = Response
    <$> (doTheThing <$> (v .:? "error") <*> (v .:? "result"))
    <*> v.: "id"

-- there's definitely a better way to do this
doTheThing (Just a) Nothing = Left a
doTheThing Nothing (Just a) = Right a

data Window = Window
    { _winLabel :: Value
    , _winId    :: Value
    } deriving (Generic, Show, Read)

makeLenses ''Window
makeLenses ''KodiInstance
makeLenses ''Method
makeLenses ''Notif
makeLenses ''Response
