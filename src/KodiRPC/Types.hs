{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module KodiRPC.Types where

import KodiRPC.Util

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
import qualified Network.HTTP.Client as HC (HttpException)
import Network.HTTP.Req as R

data RpcException
  = ProtocolException HC.HttpException -- network exceptions
  | ReqException Value -- req exceptions
  | RpcError ErrorResponse -- RPC errors
  deriving (Show)

data ErrorResponse = ErrorResponse
  { _errorCode :: Int
  , _errorMessage :: T.Text
  , _errorData :: ErrorData
  } deriving (Generic, Show, Read)

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \v -> ErrorResponse
              <$> v.:"code"
              <*> v.:"message"
              <*> v.:"data"

data ErrorData = ErrorData
  { _dataMethod :: T.Text
  , _dataStack :: ErrorStack
  } deriving (Generic, Show, Read)

instance FromJSON ErrorData where
  parseJSON = withObject "ErrorData" $ \e -> ErrorData
                <$> e.:"method"
                <*> e.:"stack"

data ErrorStack = ErrorStack
  { _stackName :: T.Text
  , _stackType :: T.Text
  , _stackMessage :: T.Text
  } deriving (Generic, Show, Read)

instance FromJSON ErrorStack where
  parseJSON = withObject "ErrorStack" $ \e -> ErrorStack
                <$> e.:"name"
                <*> e.:"type"
                <*> e.:"message"

data KodiInstance = KodiInstance
   { _server :: T.Text
   , _port   :: Int
   , _username :: T.Text
   , _password :: T.Text
   } deriving (Generic, Show)

type Params = Object

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

methodNoP = flip method' HM.empty

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

data Response = Response
  { _result :: Either ErrorResponse Value
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

instance FromJSON Window where
  parseJSON = withObject "Window" $ \w -> Window
              <$> w.:"label"
              <*> w.:"id"

makeLenses ''Window
makeLenses ''KodiInstance
makeLenses ''Method
makeLenses ''Notif
makeLenses ''Response

local = KodiInstance "localhost" 8080 "" ""
