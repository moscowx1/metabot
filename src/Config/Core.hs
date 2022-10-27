{-# LANGUAGE OverloadedStrings #-}

module Config.Core (readConfigEither, Config (..)) where

import Config.Data
  ( Info,
    ParseErr,
    RepeatNum,
    Timeout,
    Token,
    infoEither,
    repeatNumEither,
    timeoutEither,
    tokenEither,
  )
import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT, runReaderT))
import Data.Bifunctor (Bifunctor (first, second))
import Data.Ini (Ini, lookupValue)
import Data.Text (Text, unpack)

data ReadConfigErr = ParseErr ParseErr | NotFound String

data Config = Config
  { cInfo :: Info,
    cInitRC :: RepeatNum,
    cToken :: Token,
    cTimeout :: Timeout
  }
  deriving (Show, Eq)

transferErr :: (String -> Either ParseErr a) -> (String -> Either ReadConfigErr a)
transferErr f s = first ParseErr (f s)

type Lookup = ReaderT (Text -> Either String String) (Either ReadConfigErr)

read' ::
  t ->
  (String -> Either ParseErr b) ->
  ReaderT (t -> Either String String) (Either ReadConfigErr) b
read' s f2 = ask >>= \e -> ReaderT $ const $ first NotFound (e s) >>= transferErr f2

readConfig :: Lookup Config
readConfig = do
  info <- read' "info" infoEither
  initRN <- read' "repeatNum" repeatNumEither
  token <- read' "token" tokenEither
  timeout <- read' "timeout" timeoutEither
  pure $
    Config
      info
      initRN
      token
      timeout

readFn :: Ini -> Text -> Either String Text
readFn = flip (lookupValue "Bot")

readConfigEither :: Ini -> Either ReadConfigErr Config
readConfigEither ini = runReaderT readConfig (second unpack . readFn ini)