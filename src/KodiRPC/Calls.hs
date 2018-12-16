{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Calls where

import KodiRPC.Methods.Application as Application
import KodiRPC.Util
import KodiRPC.Types as Ty
import qualified KodiRPC.Methods.Input as I
import KodiRPC.Methods.GUI as Gui

import Debug.Trace
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson         as A
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B (pack)
import           Data.Default.Class
import           Data.Either as E
import           Data.HashMap.Lazy  as HM
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           Lens.Micro.Platform ((^.), (^?))
import           Network.HTTP.Req   as R
import           Network.Socket             (withSocketsDo)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

kReq :: (MonadHttp m, ToJSON a, FromJSON b) => KodiInstance -> a -> m (JsonResponse b)
kReq ki method = req POST (http url /: "jsonrpc") (ReqBodyJson method) jsonResponse options
   where p   = ki^.Ty.port
         url = ki^.server
         options = R.port p
                    <> header "Content-Type" "application/json"
                    <> R.basicAuthUnsafe (T.encodeUtf8 $ ki^.username) (T.encodeUtf8 $ ki^.password) -- kodi doesn't support tls/ssl

local = KodiInstance "localhost" 8080 "" ""

kall :: KodiInstance -> Method -> IO (Either String Response)
kall kodiInstance method = handle excpt (Right <$> kall' kodiInstance method)
   where excpt e    = return . Left $ show (e::HttpException)
         kall' ki m = runReq def $ do
            r <- kReq ki m
            return (responseBody r :: Response)

kall'' :: KodiInstance -> Method -> IO (Either RpcException Value)
kall'' ki method = joinReqResponse <$> handle excpt (Right <$> kall' ki method)
   where excpt e    = return . Left $ handleReqExcpt e
         kall' ki m = runReq def $ do
            r <- kReq ki m
            return (responseBody r :: Response)

handleReqExcpt :: R.HttpException -> RpcException
handleReqExcpt (JsonHttpException e) = ReqException $ fromMaybe (String . T.pack $ e) (decode (B.pack e) :: Maybe Value)
handleReqExcpt (R.VanillaHttpException e) = ProtocolException e

joinReqResponse :: Either RpcException Response -> Either RpcException Value
joinReqResponse res = (mapLeft RpcError . _result) =<< res

tdb x = trace (show x) x

notification :: KodiInstance -> IO (Maybe Notif)
notification ki = withSocketsDo $ WS.runClient (T.unpack $ ki^.server) 9090 "/jsonrpc" kNotifHandler
    where kNotifHandler :: WS.Connection -> IO (Maybe Notif)
          kNotifHandler conn = do
            (WS.Text msg _) <- WS.receiveDataMessage conn
            let dMsg = decode msg :: Maybe Notif
            return dMsg

-- todo: convert to returning IO (Either)
-- getWindow :: KodiInstance -> IO (Maybe Window)
getWindow ki = do
    let response = MaybeT $ do
            r <- kall ki $ Gui.getProperties [Currentwindow] -- Either String Response
            return $ join $ eitherToMaybe . _result <$> eitherToMaybe r
    runMaybeT $ do
        win <- response
        cwin <- lookup' "currentwindow" win
        label <- lookup' "label" cwin
        winId <- lookup' "id" cwin
        return $ Window label winId -- change Window to Window String Int ?
    where lookup' key (Object o) = MaybeT $ return $ HM.lookup key o
          lookup' _ _            = MaybeT $ return Nothing

smartActionMap :: Window -> I.Action -> I.Action
smartActionMap (Window "Fullscreen video"    _ ) I.Up    = I.Bigstepforward
smartActionMap (Window "Fullscreen video"    _ ) I.Down  = I.Bigstepback
smartActionMap (Window "Fullscreen video"    _ ) I.Left  = I.Stepback
smartActionMap (Window "Fullscreen video"    _ ) I.Right = I.Stepforward
smartActionMap (Window "Audio visualisation" _ ) I.Up    = I.Bigstepforward
smartActionMap (Window "Audio visualisation" _ ) I.Down  = I.Bigstepback
smartActionMap (Window "Audio visualisation" _ ) I.Left  = I.Stepback
smartActionMap (Window "Audio visualisation" _ ) I.Right = I.Stepforward
smartActionMap _                                 x       = x

smartAction :: KodiInstance -> I.Action -> IO (Either String Response)
smartAction ki action = getWindow ki >>= maybe conErr doAction
    where doAction window = kall ki $ I.executeAction $ smartActionMap window action
          conErr = return $ Left "Connection error. Received no window."

ping :: KodiInstance -> IO (Maybe KodiInstance)
ping ki = either (const Nothing) (const $ Just ki) <$> kall ki (Application.getProperties [Application.Version])
