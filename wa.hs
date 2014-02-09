{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             ScopedTypeVariables #-}

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

import Network.SimpleIRC
import System.IO

import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Utils
import Types

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

toIRC mirc =
  awaitForever $ \m@Message{..} -> do
    when (take 5 kind == "group") $
      liftIO $ sendMessage mirc m
    return m
  where
    sendMessage mirc Message{..} = do
      let nick = fromMaybe jid pushName
      let content = "<" ++ nick ++ "> " ++ msg
      sendMsg mirc "#compagnia3" $ B.pack content

main = do
  resp <- connect
          (mkDefaultConfig "irc.rizon.net" "MuhBot2")
          { cChannels = ["#compagnia3"]
          , cEvents   = [(Privmsg onMessage)]} True False

  case resp of
    Left error -> putStrLn $ show error
    Right mirc -> runResourceT $
                  sourceBeanstalk "127.0.0.1" "14711" "recv"
                  $= toRecord
                  $= toIRC mirc
                  $= CL.map (\(m :: Message) -> B.pack $ show m)
                  $$ CB.sinkHandle stdout

  return ()
