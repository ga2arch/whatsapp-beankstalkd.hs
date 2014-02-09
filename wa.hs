{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM.TBMChan
import Data.Maybe
import Control.Monad.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import Data.Conduit.Process.Unix
import Data.Conduit.TMChan
import Network.Beanstalk
import Network.SimpleIRC
import System.IO

import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

data Message = Message
               { jid :: String
               , chan :: String
               , pushName :: Maybe String
               , time :: Maybe String
               , msg :: String
               , kind :: String
               } deriving Show

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$>
    v .: "jid" <*>
    v .: "chan" <*>
    v .:? "pushName" <*>
    v .:? "time" <*>
    v .: "msg" <*>
    v .: "kind"

  parseJSON _ = mzero

data SendMessage = SendMessage
                   { target :: String
                   , text :: String
                   } deriving Show

instance ToJSON SendMessage where
  toJSON (SendMessage target text) =
    object ["target" .= target, "text" .= text]

--onMessage :: EventFunc
onMessage s m = do
  when (chan == "#compagnia3") $ do
    let j = encode $ SendMessage
            "393935167969-1391363587@g.us"
            (nick ++ ": " ++ msg)
    yield (head $ BL.toChunks j)
      $$ sinkBeanstalk  "127.0.0.1" "14711" "send"
  where
    chan = fromJust $ mChan m
    msg = B.unpack $ mMsg m
    nick = B.unpack . fromJust $ mNick m

sourceBeanstalk hostname port tube = do
  s <- liftIO $ connectBeanstalk hostname port
  liftIO $ watchTube s tube
  forever $ (liftIO $ reserveJob s) >>= yield.job_body

sinkBeanstalk hostname port tube = do
  s <- liftIO $ connectBeanstalk hostname port
  liftIO $ useTube s tube
  awaitForever $ \d -> (liftIO $ putJob s 0 0 10 d) >> return ()

conduitAeson :: (Monad m) => Conduit BI.ByteString m Message
conduitAeson =
  awaitForever $ yield . fromJust . decodeStrict

main = do
  forkIO $ runResourceT $ sourceBeanstalk "127.0.0.1" "14711" "recv"
    $= conduitAeson
    $= CL.map (B.pack . show)
    $$ CB.sinkHandle stdout

  (Right mirc) <- connect
                  (mkDefaultConfig "irc.rizon.net" "MuhBot2")
                  { cChannels = ["#compagnia3"]
                  , cEvents   = [(Privmsg onMessage)]} False False
  return ()
