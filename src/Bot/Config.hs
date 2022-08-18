{-# LANGUAGE OverloadedStrings #-}

module Bot.Config
  ( readConfig,
    -- for testing
    lookupRepeatCount,
    lookupRepeatMessage,
    lookupHelpMessage,
    lookupPort,
    lookupServerConfig,
    lookupToken,
    ServerConfig (..),
  )
where

import Bot.Data
  ( HelpMessage,
    Port,
    RepeatCount,
    RepeatMessage,
    Token,
    readHelpMessage,
    readPort,
    readRepeatCount,
    readRepeatMessage,
    readToken,
  )
import Data.Either.Combinators (mapLeft)
import Data.Ini (Ini, lookupValue, readIniFile)
import qualified Data.Text as T

type NonEmptyText = T.Text

data ServerConfig = ServerConfig
  { sPort :: Int,
    sHelpMessage :: NonEmptyText,
    sRepeatMessage :: NonEmptyText,
    sRepeatCount :: Int,
    sToken :: NonEmptyText
  }
  deriving (Show, Eq)

lookupValueT ::
  T.Text ->
  T.Text ->
  (T.Text -> Either T.Text a) ->
  Ini ->
  Either T.Text a
lookupValueT sec val fn ini = mapLeft T.pack (lookupValue sec val ini) >>= fn

lookupPort :: Ini -> Either T.Text Port
lookupPort = lookupValueT "Server" "Port" readPort

lookupBotSection :: T.Text -> (T.Text -> Either T.Text a) -> Ini -> Either T.Text a
lookupBotSection = lookupValueT "Bot"

lookupHelpMessage :: Ini -> Either T.Text HelpMessage
lookupHelpMessage = lookupBotSection "HelpMessage" readHelpMessage

lookupRepeatMessage :: Ini -> Either T.Text RepeatMessage
lookupRepeatMessage = lookupBotSection "RepeatMessage" readRepeatMessage

lookupRepeatCount :: Ini -> Either T.Text RepeatCount
lookupRepeatCount = lookupBotSection "RepeatCount" readRepeatCount

lookupToken :: Ini -> Either T.Text Token
lookupToken = lookupBotSection "Token" readToken

lookupServerConfig :: Ini -> Either T.Text ServerConfig
lookupServerConfig ini = do
  port <- lookupPort ini
  helpMsg <- lookupHelpMessage ini
  repeatMsg <- lookupRepeatMessage ini
  repeatCount <- lookupRepeatCount ini
  token <- lookupToken ini
  pure $
    ServerConfig
      { sPort = port,
        sHelpMessage = helpMsg,
        sRepeatMessage = repeatMsg,
        sRepeatCount = repeatCount,
        sToken = token
      }

readIniFileT :: FilePath -> IO (Either T.Text Ini)
readIniFileT path = do
  ini <- readIniFile path
  pure $ mapLeft T.pack ini

readConfig :: IO (Either T.Text ServerConfig)
readConfig = do
  ini <- readIniFileT "config.ini"
  let config = ini >>= lookupServerConfig
  pure config
