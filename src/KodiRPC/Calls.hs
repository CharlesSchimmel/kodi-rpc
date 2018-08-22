{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Calls where

import KodiRPC.Types as Ty
import qualified KodiRPC.Methods.Input as I
import KodiRPC.Methods.GUI

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson         as A
import           Data.Aeson.Types
import           Data.Default.Class
import           Data.Either
import           Data.HashMap.Lazy  as HM
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           Lens.Micro.Platform ((^.), (^?))
import           Network.HTTP.Req   as R
import           Network.Socket             (withSocketsDo)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

kReq :: (MonadHttp m, ToJSON a, FromJSON b) => KodiInstance -> a -> m (JsonResponse b)
kReq ki method = req POST (http url /: "jsonrpc") (ReqBodyJson method) jsonResponse (R.port p <> header "Content-Type" "application/json")
   where p   = ki^.Ty.port
         url = ki^.server

local = KodiInstance "localhost" 8080

kall :: KodiInstance -> Method -> IO Response
kall kodiInstance method = handle excpt (Right <$> kall' kodiInstance method)
   where excpt e    = return . Left $ show (e::HttpException)
         kall' ki m = runReq def $ do
            r <- kReq ki m
            return (responseBody r :: Value)

notification :: KodiInstance -> IO (Maybe Notif)
notification ki = withSocketsDo $ WS.runClient (T.unpack $ ki^.server) 9090 "/jsonrpc" kNotifHandler
    where kNotifHandler :: WS.Connection -> IO (Maybe Notif)
          kNotifHandler conn = do
            (WS.Text msg _) <- WS.receiveDataMessage conn
            let dMsg = decode msg :: Maybe Notif
            return dMsg

getWindow :: KodiInstance -> IO (Maybe Window)
getWindow ki = do
    response <- kall ki $ getProperties [Currentwindow]
    case response of
        (Right obj) -> do
            let win   = lookup' "result" obj >>= lookup' "currentwindow"
                winId = win >>= lookup' "id"
                label = win >>= lookup' "label"
            return $ Window <$> label <*> winId
        _ -> return Nothing
    where lookup' key (Object o) = HM.lookup key o
          lookup' _ _            = Nothing
          maybeStr (String s)    = Just s
          maybeStr _             = Nothing
          maybeNum (Number n)    = Just n
          maybeNum _             = Nothing

smartActionMap (Window "Fullscreen video" _)     I.Up    = I.Bigstepforward
smartActionMap (Window "Fullscreen video" _)     I.Down  = I.Bigstepback
smartActionMap (Window "Fullscreen video" _)     I.Left  = I.Stepback
smartActionMap (Window "Fullscreen video" _)     I.Right = I.Stepforward
smartActionMap (Window "Audio visualisation" _)  I.Up    = I.Bigstepforward
smartActionMap (Window "Audio visualisation" _)  I.Down  = I.Bigstepback
smartActionMap (Window "Audio visualisation" _)  I.Left  = I.Stepback
smartActionMap (Window "Audio visualisation" _)  I.Right = I.Stepforward
smartActionMap _ x = x

smartAction :: KodiInstance -> I.Action -> IO Response
smartAction ki action = do
    window <- getWindow ki
    case window of
        (Just window) -> do
            let sAct = smartActionMap window action
            result <- kall ki $ I.executeAction sAct
            return result
        _ -> return $ Left "Connection error"

