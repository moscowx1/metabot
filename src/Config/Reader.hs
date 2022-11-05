{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Reader
  ( readConfigEither,
    Config (..),
    ReadConfigErr (..),
  )
where

import Config.Data (Config (Config))
import Config.Parser
  ( infoEither,
    modeEither,
    modifyErr,
    repeatNumEither,
    timeoutEither,
    tokenEither,
  )
import Config.ParserInternal (ParseErr, Parser)
import Control.Monad.Reader (ReaderT (runReaderT), ask, lift)
import Data.Bifunctor (bimap)
import Data.Ini (Ini, lookupValue)
import Data.Text (Text, unpack)

data ReadConfigErr = ParseErr ParseErr | NotFound String
  deriving (Show)

type ConfigReader =
  ReaderT
    (Text -> Either ReadConfigErr String)
    (Either ReadConfigErr)

read' :: Text -> Parser a -> ConfigReader a
read' section parser = do
  readFn <- ask
  raw <- lift $ readFn section
  let p' = modifyErr parser ParseErr
  lift $ p' raw

readConfig :: ConfigReader Config
readConfig =
  Config
    <$> read' "info" infoEither
    <*> read' "repeatNum" repeatNumEither
    <*> read' "token" tokenEither
    <*> read' "timeout" timeoutEither
    <*> read' "mode" modeEither

reader :: Ini -> Text -> Either ReadConfigErr String
reader ini section = bimap NotFound unpack $ lookupValue "Bot" section ini

readConfigEither :: Ini -> Either ReadConfigErr Config
readConfigEither ini = runReaderT readConfig (reader ini)