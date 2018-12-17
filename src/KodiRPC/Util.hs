module KodiRPC.Util where

import Data.HashMap.Lazy as HM
import Data.Aeson
import Control.Monad
import Data.Text
import Data.Scientific
import Debug.Trace

-- Easier lookup for Objects
lookup' :: Text -> Value -> Maybe Value
lookup' key (Object map) = HM.lookup key map
lookup' _ _ = Nothing

map #? key = HM.lookup key map

(>#?) :: Maybe Value -> Text -> Maybe Value
map >#? key = map >>= lookup' key

maybeStr (Just(String s)) = Just s
maybeStr _ = Nothing

maybeNum (Just (Number n)) = Just n
maybeNum _ = Nothing

maybeBool (Just (Bool b)) = Just b
maybeBool _ = Nothing

maybeSci :: Scientific -> Maybe Float
maybeSci s = either Just (const Nothing) $ floatingOrInteger s

maybeInt :: Scientific -> Maybe Int
maybeInt s = either (const Nothing) Just $ floatingOrInteger s

mapLeft f = either (Left . f) Right
eitherToMaybe = either (const Nothing) Just

tdb x = trace (show x) x
