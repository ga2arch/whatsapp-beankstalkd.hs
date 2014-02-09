{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Applicative
import Control.Monad
import Data.Aeson

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

--------------------------------------------

data SendMessage = SendMessage
                   { target :: String
                   , text :: String
                   } deriving Show

instance ToJSON SendMessage where
  toJSON (SendMessage target text) =
    object ["target" .= target, "text" .= text]
