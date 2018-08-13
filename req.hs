{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Default.Class
import Network.HTTP.Req
import Data.Map

-- kmContextMenu      = Method 1.0 2.0 "Input.ContextMenu" empty
-- kmGetProperties    = Method 1.0 2.0 "Application.GetProperties" $ fromList [("params",["volume"])]
-- kmGuiGetProperties = Method 1.0 2.0 "GUI.GetProperties" $ fromList [("properties",[ "currentwindow"])]

-- addonsGetAddons  = Method 1.0 2.0 "Addons.GetAddons" $ fromList [("type",["xbmc.gui.skin","xbmc.python.pluginsource"])]

-- kInputBack          endpoint port = KodiRequest endpoint port kBack
-- kInputContextMenu   endpoint port = KodiRequest endpoint port kContextMenu

-- getKodi :: Method -> String -> Int -> 
getKodi method serverUrl p = req POST (http serverUrl /: "jsonrpc") (ReqBodyJson method) jsonResponse (port p <> header "Content-Type" "application/json")

-- kRight = runReq def $ do
--     r <- getKodi addonsGetAddons "192.168.1.4" 8080
--     return (responseBody r :: Value)

kDo method = runReq def $ do
    r <- getKodi method "localhost" 8080
    return (responseBody r :: Value)

-- main = do
--     result <- kRight
--     print result
