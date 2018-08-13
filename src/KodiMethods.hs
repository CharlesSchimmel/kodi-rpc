{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module KodiMethods where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import Data.Monoid
import Data.Default.Class

data Method = Method {
    id :: Double,
    jsonrpc :: Double,
    method :: String,
    params :: Params
    }
    deriving (Generic, Show)
instance ToJSON Method where
    toJSON (Method id jsonrpc method params) =
        object ["id" .= show id, "jsonrpc" .= show jsonrpc, "method" .= method , "params" .= toJSON params]

type Params = Value

data Response = Response {
   result :: String
   ,resjsonrpc :: String
   ,resId :: String
                           }
    deriving (Show)

instance FromJSON Response where
    parseJSON (Object v) = Response <$>
        v .: "result" <*>
        v .: "jsonrpc" <*>
        v .: "id"

getKodi method serverUrl kport = req POST (http serverUrl /: "jsonrpc") (ReqBodyJson method) jsonResponse (port kport <> header "Content-Type" "application/json")

kGet method = runReq def $ do
    r <- getKodi method "localhost" 8080
    return (responseBody r :: Value)
