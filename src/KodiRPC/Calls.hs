{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calls where

import Types
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

kReq :: (MonadHttp m, ToJSON a, FromJSON b) => KodiInstance -> a -> m (JsonResponse b)
kReq ki method = req POST (http url /: "jsonrpc") (ReqBodyJson method) jsonResponse (R.port p <> header "Content-Type" "application/json")
   where p   = ki^.Types.port
         url = ki^.server

kall :: (ToJSON a) => KodiInstance -> a -> IO (Either String Value)
kall kodiInstance method = handle excpt (Right <$> kall' kodiInstance method)
   where excpt e    = return . Left $ show (e::HttpException)
         kall' ki m = runReq def $ do
            r <- kReq ki m
            return (responseBody r :: Value)
