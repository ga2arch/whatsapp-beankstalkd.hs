{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             ScopedTypeVariables
             #-}

import Control.Concurrent hiding (yield)
import Control.Exception
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import Network.SimpleIRC
import System.IO

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Utils
import Types

chans = [("#canaleprova", "393935167969-1391363587@g.us")
        ,("#compagnia2", "393477436444-1382908121@g.us")
        ,("#canea", "393477436444-1365363106@g.us")]

onMessage s m = do
  let mjid = lookup chan chans

  case mjid of
    Just jid -> do
      let j = encode $
              SendMessage jid  ("<" ++ nick ++ "> " ++ msg)
      yield (head $ BL.toChunks j)
        $$ sinkBeanstalk  "127.0.0.1" "14711" "send"

    Nothing -> return ()
  where
    chan = fromJust $ mChan m
    msg = B.unpack $ mMsg m
    nick = B.unpack . fromJust $ mNick m

toIRC config = do
  resp <- liftIO $ connect config True False

  case resp of
    Left error -> do
      liftIO $ putStrLn $ show error
      awaitForever $ \m -> yield m

    Right mirc -> do
      awaitForever $ \m@Message{..} -> do
        when (take 5 kind == "group") $
          liftIO $ sendMessage mirc m
        yield m

  where
    sendMessage mirc Message{..} = do
      let mname = lookup chan $ map (\(a,b) -> (b,a)) chans

      case mname of
        Just name -> do
          let nick = fromMaybe jid pushName
          let content = "<" ++ nick ++ "> " ++ msg
          sendMsg mirc name $ B.pack content

        Nothing -> return ()

main = do
  let config = (mkDefaultConfig "irc.rizon.net" "MuhBot")
               { cChannels = map (B.unpack.fst) chans
               , cEvents   = [(Privmsg onMessage)]}

  runResourceT $
    sourceBeanstalk "127.0.0.1" "14711" "recv"
    $= toRecord
    $= toIRC config
    $= CL.map (\(m :: Message) -> B.pack $ show m)
    $$ CB.sinkHandle stdout

  `catch` \(e :: SomeException) -> main
