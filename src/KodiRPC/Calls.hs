{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Calls where

import KodiRPC.Types as Ty
import qualified KodiRPC.Methods.Input as I
import KodiRPC.Methods.GUI

import Debug.Trace
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
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

kall :: KodiInstance -> Method -> IO (Either String Response)
kall kodiInstance method = handle excpt (Right <$> kall' kodiInstance method)
   where excpt e    = return . Left $ show (e::HttpException)
         kall' ki m = runReq def $ do
            r <- kReq ki m
            return $ tdb (responseBody r :: Response)

tdb x = trace (show x) x

notification :: KodiInstance -> IO (Maybe Notif)
notification ki = withSocketsDo $ WS.runClient (T.unpack $ ki^.server) 9090 "/jsonrpc" kNotifHandler
    where kNotifHandler :: WS.Connection -> IO (Maybe Notif)
          kNotifHandler conn = do
            (WS.Text msg _) <- WS.receiveDataMessage conn
            let dMsg = decode msg :: Maybe Notif
            return dMsg

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- getWindow :: KodiInstance -> IO (Maybe Window)
getWindow ki = do
    response <- kall ki $ getProperties [Currentwindow]
    let maybeRes = eitherToMaybe response
        val = join $ eitherToMaybe <$> _result <$> maybeRes
    let win = lookup' "currentwindow" =<< val
        label = lookup' "label" =<< win
        winId = lookup' "id" =<< win
    return $ Window <$> label <*> winId
    where lookup' key (Object o) = HM.lookup key o
          lookup' _ _            = Nothing

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
smartAction ki action = do
    window <- getWindow ki
    case window of
        (Just window) -> 
            kall ki $ I.executeAction $ smartActionMap window action
        _ -> 
            return $ Left "Connection error. Received no window."
