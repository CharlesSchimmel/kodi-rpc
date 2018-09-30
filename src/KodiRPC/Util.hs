module KodiRPC.Util where

import Data.HashMap.Lazy as HM
import Data.Aeson
import Control.Monad

-- Aeson convenience methods

-- Easier lookup for Objects
lookup' :: Text -> Value -> Maybe Value
lookup' key (Object map) = HM.lookup key map
lookup' _ _ = Nothing

(#?) :: HashMap k v -> k -> Maybe v
map #? key = HM.lookup key map

(>#?) :: Maybe Value -> Text -> Maybe Value
map >#? key = map >>= lookup' key

maybeStr (Just(String s)) = Just s
maybeStr _ = Nothing

maybeNum (Just (Number n)) = Just n
maybeNum _ = Nothing

maybeSci :: Scientific -> Maybe Float
maybeSci s = either Just (const Nothing) $ floatingOrInteger s
