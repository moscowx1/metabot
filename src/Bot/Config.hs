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
    lookupTimeout,
    lookupOffset,
    ServerConfig (..),
  )
where

import Bot.Data
  ( HelpMessage,
    Offset,
    Port,
    RepeatCount,
    RepeatMessage,
    Timeout,
    Token,
    readHelpMessage,
    readInitOffset,
    readPort,
    readRepeatCount,
    readRepeatMessage,
    readTimeout,
    readToken,
  )
import Data.Either.Combinators (mapLeft)
import Data.Ini (Ini, lookupValue, readIniFile)
import qualified Data.Text as T

data ServerConfig = ServerConfig
  { sPort :: Port,
    sHelpMessage :: HelpMessage,
    sRepeatMessage :: RepeatMessage,
    sRepeatCount :: RepeatCount,
    sToken :: Token,
    sTimeout :: Int,
    sInitialOffset :: Offset
  }
  deriving (Show, Eq)

lookupText :: T.Text -> T.Text -> Ini -> Either T.Text T.Text
lookupText sec val ini = mapLeft T.pack (lookupValue sec val ini)

lookupData ::
  T.Text -> -- Section
  T.Text -> -- Key
  (T.Text -> Either T.Text a) ->
  Ini ->
  Either T.Text a
lookupData sec val fn ini = lookupText sec val ini >>= fn

lookupPort :: Ini -> Either T.Text Port
lookupPort = lookupData "Server" "Port" readPort

lookupBotSection :: T.Text -> (T.Text -> Either T.Text a) -> Ini -> Either T.Text a
lookupBotSection = lookupData "Bot"

lookupHelpMessage :: Ini -> Either T.Text HelpMessage
lookupHelpMessage = lookupBotSection "HelpMessage" readHelpMessage

lookupRepeatMessage :: Ini -> Either T.Text RepeatMessage
lookupRepeatMessage = lookupBotSection "RepeatMessage" readRepeatMessage

lookupRepeatCount :: Ini -> Either T.Text RepeatCount
lookupRepeatCount = lookupBotSection "RepeatCount" readRepeatCount

lookupToken :: Ini -> Either T.Text Token
lookupToken = lookupBotSection "Token" readToken

lookupTimeout :: Ini -> Either T.Text Timeout
lookupTimeout = lookupBotSection "Timeout" readTimeout

lookupOffset :: Ini -> Either T.Text Offset
lookupOffset ini = case text of
  Nothing -> pure Nothing
  Just t -> readInitOffset t
  where
    text = either (const Nothing) Just $ lookupText "Bot" "InitOffset" ini

lookupServerConfig :: Ini -> Either T.Text ServerConfig
lookupServerConfig ini = do
  port <- lookupPort ini
  helpMsg <- lookupHelpMessage ini
  repeatMsg <- lookupRepeatMessage ini
  repeatCount <- lookupRepeatCount ini
  token <- lookupToken ini
  timeout <- lookupTimeout ini
  offset <- lookupOffset ini
  pure $
    ServerConfig
      { sPort = port,
        sHelpMessage = helpMsg,
        sRepeatMessage = repeatMsg,
        sRepeatCount = repeatCount,
        sToken = token,
        sTimeout = timeout,
        sInitialOffset = offset
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
