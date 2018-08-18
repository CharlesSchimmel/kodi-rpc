{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Calls where

import KodiRPC.Types as Ty

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Conduit              as C
import           Data.Conduit.Combinators  as CC
import           Data.Default.Class
import           Data.Either
import           Data.HashMap.Strict       as HM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           GHC.Generics
import           Lens.Micro.Platform ((^.))
import           Network.HTTP.Req          as R
import           Network.Socket             (withSocketsDo)
import qualified Network.WebSockets        as WS

kReq :: (MonadHttp m, ToJSON a, FromJSON b) => KodiInstance -> a -> m (JsonResponse b)
kReq ki method = req POST (http url /: "jsonrpc") (ReqBodyJson method) jsonResponse (R.port p <> header "Content-Type" "application/json")
   where p   = ki^.Ty.port
         url = ki^.server


kall :: (ToJSON a) => KodiInstance -> a -> IO (Either String Value)
kall kodiInstance method = handle excpt (Right <$> kall' kodiInstance method)
   where excpt e    = return . Left $ show (e::HttpException)
         kall' ki m = runReq def $ do
            r <- kReq ki m
            return (responseBody r :: Value)

kNotifHandler conn = do
    (WS.Text msg _) <- WS.receiveDataMessage conn
    let dMsg = decode msg :: Maybe Notif
    return dMsg

kNotifGetter conn = do
    (WS.Text msg _) <- WS.receiveDataMessage conn
    let dMsg = decode msg :: Maybe Notif
    return dMsg

buildList o n = o ++ [n]

kNotif :: KodiInstance -> IO (Maybe Notif)
kNotif ki = withSocketsDo $ WS.runClient (T.unpack $ ki^.server) 9090 "/jsonrpc" kNotifHandler

htpc = KodiInstance "192.168.1.4" 8080

kNotif' :: IO (Maybe Notif)
kNotif' = kNotif htpc

kNotifConduit :: KodiInstance -> ConduitT () (Maybe Notif) IO ()
kNotifConduit ki = repeatM (kNotif ki)

notifStream :: KodiInstance -> IO [Maybe Notif]
notifStream ki = ioCons kn $ notifStream ki
    where kn = kNotif ki
          ioCons = liftM2 (:)

mCons :: (Monad m) => m a -> m [a] -> m [a]
mCons = liftM2 (:)

notifStream' = notifStream htpc
