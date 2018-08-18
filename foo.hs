
import Data.Conduit
import Control.Monad

f :: (Monad m) => m Int
f = forever $ do
    return 1

streamish :: (Monad m) => m [Int]
streamish = ioCons f streamish

ioCons :: (Monad m) => m a -> m [a] -> m [a]
ioCons = liftM2 (:)

