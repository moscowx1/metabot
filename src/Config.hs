{-# LANGUAGE OverloadedStrings #-}

module Config
  ( readRepeatMessage,
    readHelpMessage,
    readRepeatCount,
    readToken,
    readInitOffset,
    readTimeout,
    readConfig,
    lookupRepeatCount,
    lookupRepeatMessage,
    lookupHelpMessage,
    lookupServerConfig,
    lookupToken,
    lookupTimeout,
    lookupOffset,
    ServerConfig (..),
    Handle (..),
    RepeatMessage,
    HelpMessage,
    ServerState (..),
    RepeatCount,
    Token,
    Offset,
    ChatId,
    Timeout,
    initHandle,
  )
where

import Data.Either.Combinators (mapRight)
import Data.Ini (Ini, lookupValue, readIniFile)
import Data.Text (Text, unpack)
import Text.Read (readEither)

readNonEmpty :: String -> String -> Either String String
readNonEmpty err t =
  if null t
    then Left err
    else Right t

type RepeatMessage = String

readRepeatMessage :: String -> Either String RepeatMessage
readRepeatMessage = readNonEmpty "repeat message cannot be empty"

type RepeatCount = Int

readRepeatCount :: String -> Either String RepeatCount
readRepeatCount t = do
  t' <- readEither t
  if t' <= 0 || t' > 5
    then Left "repeat count should be between 1 and 5"
    else Right t'

type HelpMessage = String

readHelpMessage :: String -> Either String HelpMessage
readHelpMessage = readNonEmpty "help message cannot be empty"

type Token = String

readToken :: String -> Either String Token
readToken = readNonEmpty "token cannot be empty"

type Timeout = Int

readTimeout :: String -> Either String Timeout
readTimeout t = do
  timeout <- readEither t
  if timeout < 9 || timeout > 1000
    then Left "timeout shoudl be between 10 and 1000"
    else Right timeout

type Offset = Int

readInitOffset :: String -> Either String (Maybe Offset)
readInitOffset t = do
  if null t
    then pure Nothing
    else do
      offset <- readEither t
      if offset <= 0
        then Left "offset shoudl be more than zero"
        else Right $ Just offset

type ChatId = Integer

data ServerState = ServerState
  { ssOffset :: Maybe Offset,
    ssChatRepeatCount :: [(ChatId, RepeatCount)]
  }

initHandle :: ServerConfig -> Handle
initHandle config =
  Handle
    { hState =
        ServerState
          { ssChatRepeatCount = [],
            ssOffset = sInitialOffset config
          },
      hConfig = config
    }

data ServerConfig = ServerConfig
  { sHelpMessage :: HelpMessage,
    sRepeatMessage :: RepeatMessage,
    sRepeatCount :: RepeatCount,
    sToken :: Token,
    sTimeout :: Timeout,
    sInitialOffset :: Maybe Offset
  }
  deriving (Show, Eq)

lookupText :: Text -> Text -> Ini -> Either String String
lookupText sec val ini = mapRight unpack (lookupValue sec val ini)

lookupData ::
  Text -> -- Section
  Text -> -- Key
  (String -> Either String a) ->
  Ini ->
  Either String a
lookupData sec val fn ini = lookupText sec val ini >>= fn

lookupBotSection :: Text -> (String -> Either String a) -> Ini -> Either String a
lookupBotSection = lookupData "Bot"

lookupHelpMessage :: Ini -> Either String HelpMessage
lookupHelpMessage = lookupBotSection "HelpMessage" readHelpMessage

lookupRepeatMessage :: Ini -> Either String RepeatMessage
lookupRepeatMessage = lookupBotSection "RepeatMessage" readRepeatMessage

lookupRepeatCount :: Ini -> Either String RepeatCount
lookupRepeatCount = lookupBotSection "RepeatCount" readRepeatCount

lookupToken :: Ini -> Either String Token
lookupToken = lookupBotSection "Token" readToken

lookupTimeout :: Ini -> Either String Timeout
lookupTimeout = lookupBotSection "Timeout" readTimeout

lookupOffset :: Ini -> Either String (Maybe Offset)
lookupOffset ini = case text of
  Nothing -> pure Nothing
  Just t -> readInitOffset t
  where
    text = either (const Nothing) Just $ lookupText "Bot" "InitOffset" ini

lookupServerConfig :: Ini -> Either String ServerConfig
lookupServerConfig ini = do
  helpMsg <- lookupHelpMessage ini
  repeatMsg <- lookupRepeatMessage ini
  repeatCount <- lookupRepeatCount ini
  token <- lookupToken ini
  timeout <- lookupTimeout ini
  offset <- lookupOffset ini
  pure $
    ServerConfig
      { sHelpMessage = helpMsg,
        sRepeatMessage = repeatMsg,
        sRepeatCount = repeatCount,
        sToken = token,
        sTimeout = timeout,
        sInitialOffset = offset
      }

readConfig :: IO (Either String ServerConfig)
readConfig = do
  ini <- readIniFile "config.ini"
  let config = ini >>= lookupServerConfig
  pure config

data Handle = Handle
  { hConfig :: ServerConfig,
    hState :: ServerState
  }
