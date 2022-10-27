{-# LANGUAGE OverloadedStrings #-}

module Config.Lookups () where

import Config.Data (Config, Info, RepeatNum, Timeout, Token)
import Control.Lens (Identity)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.Trans.Except (except)
import Data.Ini (Ini, lookupValue)
import Data.Text (Text)

type Section = Text

type Key = Text

--data LookupErr = LookupErr String
--
--lookupText :: Text -> Text -> Ini -> Either String String
--lookupText sec val ini = mapRight unpack (lookupValue sec val ini)
--
--lookupData ::
--  Text -> -- Section
--  Text -> -- Key
--  (String -> Either String a) ->
--  Ini ->
--  Either String a
--lookupData sec val fn ini = lookupText sec val ini >>= fn
--
--lookupBotSection :: Text -> (String -> Either String a) -> Ini -> Either String a
--lookupBotSection = lookupData "Bot"
--
--lookupBs = flip (lookupValue "Bot")
--
--lookupInfo' :: Lookup String
--lookupInfo' = do
--  ini <- ask
--  except $ lookupBs ini "Info"
--
--lookupInfo :: Ini -> Either String Info
--lookupInfo = lookupBotSection "HelpMessage" readHelpMessage
--
--lookupRepeatCount :: Ini -> Either String RepeatNum
--lookupRepeatCount = lookupBotSection "RepeatCount" readRepeatCount
--
--lookupToken :: Ini -> Either String Token
--lookupToken = lookupBotSection "Token" readToken
--
--lookupTimeout :: Ini -> Either String Timeout
--lookupTimeout = lookupBotSection "Timeout" readTimeout
--
--type Lookup = ReaderT Ini (ExceptT LookupErr Identity)
--
--lookupServerConfig :: Ini -> Either String Config
--lookupServerConfig ini = do
--  info <- lookupInfo ini
--  defRC <- lookupRepeatCount ini
--  token <- lookupToken ini
--  timeout <- lookupTimeout ini
--  pure $
--    Config
--      info
--      defRC
--      token
--      timeout