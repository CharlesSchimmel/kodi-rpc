{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Default.Class
import Data.Either
import Data.Monoid
import Data.Text as T
import GHC.Generics
import Lens.Micro.Platform hiding ((.=))
import Network.HTTP.Req as R

type Params = Value

-- Basic components of a RPC Method
data Method = Method 
   { _methodId      :: Double
   , _methodJsonrpc :: Double
   , _methodStr     :: String
   , _params        :: Params
   } deriving (Generic, Show)

instance ToJSON Method where
   toJSON (Method id jsonrpc method params) = object [
               "id"      .= show id
             , "jsonrpc" .= show jsonrpc
             , "method"  .= method
             , "params"  .= toJSON params
             ]

data Response = Response
   { _resId      :: String
   , _resJsonrpc :: String
   , _result     :: Value
   } deriving (Generic, Show)

instance FromJSON Response where
   parseJSON = withObject "Response" $ \v -> Response
      <$> v.: "id"
      <*> v.: "jsonrpc"
      <*> v.: "result"

data KodiInstance = KodiInstance
   { _server :: T.Text
   , _port   :: Int
   } deriving (Generic, Show)

makeLenses ''KodiInstance
makeLenses ''Method

