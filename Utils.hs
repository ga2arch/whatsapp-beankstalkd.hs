module Utils where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import Data.Maybe
import Network.Beanstalk

import qualified Data.ByteString.Internal as BI

sourceBeanstalk hostname port tube = do
  s <- liftIO $ connectBeanstalk hostname port
  liftIO $ watchTube s tube
  forever $ do
    job <- liftIO $ reserveJob s
    liftIO $ deleteJob s $ job_id job
    yield $ job_body job

sinkBeanstalk hostname port tube = do
  s <- liftIO $ connectBeanstalk hostname port
  liftIO $ useTube s tube
  awaitForever $
    \d ->
    (liftIO $ putJob s 0 0 10 d)
    >> return ()

toRecord :: (Monad m, FromJSON b) => Conduit BI.ByteString m b
toRecord =
  awaitForever $ yield . fromJust . decodeStrict
