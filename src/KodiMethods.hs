{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KodiMethods where

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
import Network.HTTP.Req
import Network.HTTP.Req as R (port)

type Params = Value

data Method = Method {
    _id :: Double,
    _jsonrpc :: Double,
    _method :: String,
    _params :: Params
                     }
    deriving (Generic, Show)

instance ToJSON Method where
   toJSON (Method id jsonrpc method params) = object [
               "id"     .= show id
             , "jsonrpc".= show jsonrpc
             , "method" .= method
             , "params" .= toJSON params
             ]

data Response = Response {
                          result :: String
                         ,resjsonrpc :: String
                         ,resId :: String
                         }
    deriving (Show, Generic)

instance FromJSON Response where
    parseJSON (Object v) = Response <$>
        v .: "result" <*>
        v .: "jsonrpc" <*>
        v .: "id"

data KodiInstance = KodiInstance {
                                  _server :: T.Text
                                 , _port :: Int
                                 }
                                 deriving (Generic, Show)

makeLenses ''KodiInstance
makeLenses ''Method

kReq :: (MonadHttp m, ToJSON a, FromJSON b) => KodiInstance -> a -> m (JsonResponse b)
kReq ki method = req POST (http url /: "jsonrpc") (ReqBodyJson method) jsonResponse (R.port p <> header "Content-Type" "application/json")
   where p = ki^.KodiMethods.port
         url = ki^.server

kall :: (ToJSON a) => KodiInstance -> a -> IO (Either String Value)
kall kodiInstance method = handle excpt (Right <$> kall' kodiInstance method)
   where excpt :: (Monad m) => HttpException -> m (Either String b)
         excpt = return . Left . show
         kall' kodiInstance method = runReq def $ do
            r <- kReq kodiInstance method
            return (responseBody r :: Value)
