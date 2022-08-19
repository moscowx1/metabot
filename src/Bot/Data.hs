{-# LANGUAGE OverloadedStrings #-}

module Bot.Data
  ( readPort,
    readRepeatMessage,
    readHelpMessage,
    readRepeatCount,
    readToken,
    readInitOffset,
    readTimeout,
    Port,
    RepeatMessage,
    HelpMessage,
    RepeatCount,
    Token,
    Offset,
    Timeout,
  )
where

import Data.Either.Combinators (mapLeft)
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

type Port = Int

readPort :: T.Text -> Either T.Text Port
readPort t = do
  port <- readEither' t
  if port < 50 || port > 10000
    then Left "port should be between 50 and 10000"
    else Right port

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

type Offset = Maybe Int

readInitOffset :: T.Text -> Either T.Text Offset
readInitOffset t = do
  if T.null t
    then pure Nothing
    else do
      offset <- readEither' t
      if offset <= 0
        then Left "offset shoudl be more than zero"
        else Right $ Just offset
