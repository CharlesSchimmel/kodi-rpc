{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Calls where

import KodiRPC.Methods.Application as Application
import KodiRPC.Util
import KodiRPC.Types.Base as Ty
import KodiRPC.Methods.GUI as Gui
import qualified KodiRPC.Methods.Input as I

import           Control.Exception
import           Data.Aeson         as A
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B (pack)
import           Data.Maybe
import           Lens.Micro.Platform ((^.))
import           Network.HTTP.Req   as R
import           Network.Socket             (withSocketsDo)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS

kReq :: (MonadHttp m, ToJSON a, FromJSON b) => KodiInstance -> a -> m (JsonResponse b)
kReq ki method = req POST (http url /: "jsonrpc") (ReqBodyJson method) jsonResponse options
   where p   = ki^.Ty.port
         url = ki^.server
         options = R.port p
                    <> header "Content-Type" "application/json"
                    <> R.basicAuthUnsafe (T.encodeUtf8 $ ki^.username) (T.encodeUtf8 $ ki^.password) -- kodi doesn't support tls/ssl

kall :: KodiInstance -> Method -> IO RpcResult
kall ki method = joinReqResponse <$> handle excpt (Right <$> kall' ki method)
   where kall' ki m = runReq defaultHttpConfig $ do
            r <- kReq ki m
            return (responseBody r :: RpcResponse)
         excpt e    = return . Left $ handleReqExcpt e
         -- join the Rpc-level errors (that RpcResult holds) into the overall error stack
         joinReqResponse :: Either RpcException RpcResponse -> Either RpcException Value
         joinReqResponse res = (mapLeft RpcError . _result) =<< res
         -- Convert req errors into errors that fit into our exception stack
         -- todo, probably unecessary
         handleReqExcpt :: R.HttpException -> RpcException
         handleReqExcpt (JsonHttpException e) = ReqException $ fromMaybe (String . T.pack $ e) (decode (B.pack e) :: Maybe Value)
         handleReqExcpt (R.VanillaHttpException e) = ProtocolException e

notification :: KodiInstance -> IO (Maybe Notif)
notification ki = withSocketsDo $ WS.runClient (T.unpack $ ki^.server) 9090 "/jsonrpc" kNotifHandler
    where kNotifHandler :: WS.Connection -> IO (Maybe Notif)
          kNotifHandler conn = do
            (WS.Text msg _) <- WS.receiveDataMessage conn
            let eitherMsg = eitherDecode msg
            let dMsg = either (const Nothing) Just eitherMsg
            return dMsg

getWindow :: KodiInstance -> IO (Either RpcException Window)
getWindow ki = do
    win <- kall ki $ Gui.getProperties [Currentwindow] -- Either RpcExcpt Value
    return $ mapLeft (ReqException . String . T.pack) =<< parseEither lookupWin <$> win
    where lookupWin = withObject "Window" $ \o -> o .: "currentwindow" :: Parser Window

getWindow' :: Monad m => Kaller m -> m (Either RpcException Window)
getWindow' kallr = do
    win <- kallr $ Gui.getProperties [Currentwindow] -- Either RpcExcpt Value
    return $ mapLeft (ReqException . String . T.pack) =<< parseEither lookupWin <$> win
    where lookupWin = withObject "Window" $ \o -> o .: "currentwindow" :: Parser Window

doGetWindow' ki = getWindow' (kall ki)

smartActionMap :: Window -> I.Action -> I.Action
smartActionMap (Window "Fullscreen video"    _ ) I.Up     = I.Bigstepforward
smartActionMap (Window "Fullscreen video"    _ ) I.Down   = I.Bigstepback
smartActionMap (Window "Fullscreen video"    _ ) I.Left   = I.Stepback
smartActionMap (Window "Fullscreen video"    _ ) I.Right  = I.Stepforward
smartActionMap (Window "Fullscreen video"    _ ) I.Select = I.Osd
smartActionMap (Window "Audio visualisation" _ ) I.Up     = I.Bigstepforward
smartActionMap (Window "Audio visualisation" _ ) I.Down   = I.Bigstepback
smartActionMap (Window "Audio visualisation" _ ) I.Left   = I.Stepback
smartActionMap (Window "Audio visualisation" _ ) I.Right  = I.Stepforward
smartActionMap (Window "Audio visualisation" _ ) I.Select = I.Osd
smartActionMap _                                 x        = x

-- | Perform appropriate actions depending on the window context. Eg while in a video, left jumps back
smartAction' :: Monad m =>
                Kaller m ->
                Window ->
                I.Action ->
                m RpcResult
smartAction' kallr window action = kallr . I.executeAction $ smartActionMap window action

smartAction'' :: Monad m =>
                 Kaller m ->
                 (Kaller m -> m (Either RpcException Window)) ->
                 I.Action ->
                 m RpcResult
smartAction'' kallr windowr action = windowr kallr >>= either (pure . Left) smAct
    where smAct win = smartAction' kallr win action

doSmartAction :: KodiInstance -> I.Action -> IO RpcResult
doSmartAction ki action = getWindow' kallr >>= either (pure . Left) smAct
    where kallr = kall ki
          smAct win = smartAction' kallr win action

-- | Perform appropriate actions depending on the window context. Eg while in a video, left jumps back
smartAction :: KodiInstance -> I.Action -> IO RpcResult
smartAction ki action = getWindow ki >>= either (pure . Left) doAction
    where doAction window = kall ki $ I.executeAction $ smartActionMap window action

ping :: KodiInstance -> IO (Maybe KodiInstance)
ping ki = either (const Nothing) (const $ Just ki) <$> kall ki (Application.getProperties [Application.Version])
