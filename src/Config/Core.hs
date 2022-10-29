{-# LANGUAGE OverloadedStrings #-}

module Config.Core
  ( readConfigEither,
    Config (..),
    ReadConfigErr (..),
  )
where

import Config.Data
  ( Info,
    ParseErr,
    Parser,
    RepeatNum,
    Timeout,
    Token,
    infoEither,
    repeatNumEither,
    timeoutEither,
    tokenEither,
  )
import Control.Monad.Reader
  ( ReaderT (runReaderT),
    ask,
    lift,
  )
import Data.Bifunctor (Bifunctor (first, second))
import Data.Ini (Ini, lookupValue)
import Data.Text (Text, unpack)

data ReadConfigErr = ParseErr ParseErr | NotFound String
  deriving (Show)

data Config = Config
  { cInfo :: Info,
    cInitRC :: RepeatNum,
    cToken :: Token,
    cTimeout :: Timeout
  }
  deriving (Show, Eq)

transferErr :: Parser a -> (String -> Either ReadConfigErr a)
transferErr f s = first ParseErr (f s)

type ConfigReader = ReaderT (Text -> Either String String) (Either ReadConfigErr)

read' ::
  Text ->
  Parser a ->
  ConfigReader a
read' s f2 = ask >>= \e -> lift $ first NotFound (e s) >>= transferErr f2

readConfig :: ConfigReader Config
readConfig =
  Config
    <$> read' "info" infoEither
    <*> read' "repeatNum" repeatNumEither
    <*> read' "token" tokenEither
    <*> read' "timeout" timeoutEither

readFn :: Ini -> Text -> Either String Text
readFn = flip (lookupValue "Bot")

readConfigEither :: Ini -> Either ReadConfigErr Config
readConfigEither ini = runReaderT readConfig (second unpack . readFn ini)