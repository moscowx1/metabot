{-# LANGUAGE OverloadedStrings #-}

module Data.Internal
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
  )
where

import Data.Either.Combinators (mapLeft)
import Data.Ini (Ini, lookupValue, readIniFile)
import qualified Data.Text as T
import Text.Read (readEither)

readNonEmpty :: T.Text -> T.Text -> Either T.Text T.Text
readNonEmpty err t =
  if T.null t
    then Left err
    else Right t

readEither' :: (Read a) => T.Text -> Either T.Text a
readEither' t = mapLeft T.pack $ readEither $ T.unpack t

type RepeatMessage = T.Text

readRepeatMessage :: T.Text -> Either T.Text RepeatMessage
readRepeatMessage = readNonEmpty "repeat message cannot be empty"

type RepeatCount = Int

readRepeatCount :: T.Text -> Either T.Text RepeatCount
readRepeatCount t = do
  t' <- readEither' t
  if t' <= 0 || t' > 5
    then Left "repeat count should be between 1 and 5"
    else Right t'

type HelpMessage = T.Text

readHelpMessage :: T.Text -> Either T.Text HelpMessage
readHelpMessage = readNonEmpty "help message cannot be empty"

type Token = T.Text

readToken :: T.Text -> Either T.Text Token
readToken = readNonEmpty "token cannot be empty"

type Timeout = Int

readTimeout :: T.Text -> Either T.Text Timeout
readTimeout t = do
  timeout <- readEither' t
  if timeout < 9 || timeout > 1000
    then Left "timeout shoudl be between 10 and 1000"
    else Right timeout

type Offset = Int

readInitOffset :: T.Text -> Either T.Text (Maybe Offset)
readInitOffset t = do
  if T.null t
    then pure Nothing
    else do
      offset <- readEither' t
      if offset <= 0
        then Left "offset shoudl be more than zero"
        else Right $ Just offset

type ChatId = Int

data ServerState = ServerState
  { ssOffset :: Maybe Offset,
    ssChatRepeatCount :: [(ChatId, RepeatCount)]
  }

initServerState config =
  ServerState
    { ssOffset = sInitialOffset config,
      ssChatRepeatCount = []
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

lookupText :: T.Text -> T.Text -> Ini -> Either T.Text T.Text
lookupText sec val ini = mapLeft T.pack (lookupValue sec val ini)

lookupData ::
  T.Text -> -- Section
  T.Text -> -- Key
  (T.Text -> Either T.Text a) ->
  Ini ->
  Either T.Text a
lookupData sec val fn ini = lookupText sec val ini >>= fn

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

lookupOffset :: Ini -> Either T.Text (Maybe Offset)
lookupOffset ini = case text of
  Nothing -> pure Nothing
  Just t -> readInitOffset t
  where
    text = either (const Nothing) Just $ lookupText "Bot" "InitOffset" ini

lookupServerConfig :: Ini -> Either T.Text ServerConfig
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

readIniFileT :: FilePath -> IO (Either T.Text Ini)
readIniFileT path = do
  ini <- readIniFile path
  pure $ mapLeft T.pack ini

readConfig :: IO (Either T.Text ServerConfig)
readConfig = do
  ini <- readIniFileT "config.ini"
  let config = ini >>= lookupServerConfig
  pure config

data Handle = Handle
  { hConfig :: ServerConfig,
    hState :: ServerState
  }
