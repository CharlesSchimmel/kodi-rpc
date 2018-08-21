{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Calls where

import KodiRPC.Types as Ty
import KodiRPC.Methods.Input (Action)
import KodiRPC.Methods.GUI (getWindow,getProperties)

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Default.Class
import           Data.Either
import           Data.HashMap.Strict       as HM
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           Lens.Micro.Platform ((^.), (^?))
import           Network.HTTP.Req          as R
import           Network.Socket             (withSocketsDo)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Network.WebSockets        as WS

kReq :: (MonadHttp m, ToJSON a, FromJSON b) => KodiInstance -> a -> m (JsonResponse b)
kReq ki method = req POST (http url /: "jsonrpc") (ReqBodyJson method) jsonResponse (R.port p <> header "Content-Type" "application/json")
   where p   = ki^.Ty.port
         url = ki^.server


kall :: KodiInstance -> Method -> IO Response
kall kodiInstance method = handle excpt (Right <$> kall' kodiInstance method)
   where excpt e    = return . Left $ show (e::HttpException)
         kall' ki m = runReq def $ do
            r <- kReq ki m
            return (responseBody r :: Value)

-- listen for next notification
kNotif :: KodiInstance -> IO (Maybe Notif)
kNotif ki = withSocketsDo $ WS.runClient (T.unpack $ ki^.server) 9090 "/jsonrpc" kNotifHandler
    where kNotifHandler :: WS.Connection -> IO (Maybe Notif)
          kNotifHandler conn = do
            (WS.Text msg _) <- WS.receiveDataMessage conn
            let dMsg = decode msg :: Maybe Notif
            return dMsg

-- -- smartAction :: KodiInstance -> Action -> IO Response
-- smartAction ki action = do
--     (Right win) <- kall ki getWindow
--     case win of
--         Object s -> do
--             let result = s ^? key "result"
--             print result
--             print s
--             return result
--         _ -> return Nothing
