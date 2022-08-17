{-# LANGUAGE OverloadedStrings #-}

module Bot.Data
  ( readPort,
    readRepeatMessage,
    readHelpMessage,
    readRepeatCount,
    Port,
    RepeatMessage,
    HelpMessage,
    RepeatCount,
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
    then Left "repeat count should be less then zero or more than five"
    else Right t'

type HelpMessage = T.Text

readHelpMessage :: T.Text -> Either T.Text HelpMessage
readHelpMessage = readNonEmpty "help message cannot be empty"

type Port = Int

readPort :: T.Text -> Either T.Text Port
readPort t = do
  t' <- readEither' t
  if t' < 50 || t' > 10000
    then Left "port should be less then 50 or more then 10k"
    else Right t'
