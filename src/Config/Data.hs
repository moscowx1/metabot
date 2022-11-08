{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.Data
  ( RepeatNum (..),
    Info (..),
    Token (..),
    Timeout (..),
    Mode (..),
    Config (..),
    HasInfo (..),
  )
where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text, pack)
import Servant.API (ToHttpApiData (toQueryParam, toUrlPiece))

newtype RepeatNum = RepeatNum {unRepeatNum :: Int}
  deriving (Show, Eq, Enum, Num)

newtype Info = Info {unInfo :: NonEmpty Char}
  deriving (Show, Eq)

class HasInfo a where
  getInfo :: a -> String

instance HasInfo Info where
  getInfo = toList . unInfo

newtype Token = Token {unToken :: NonEmpty Char}
  deriving (Show, Eq)

instance ToHttpApiData Token where
  toUrlPiece = pack . toList . unToken

newtype Timeout = Timeout {unTimeout :: Int}
  deriving (Show, Eq)

intToText :: Int -> Text
intToText = pack . show

instance ToHttpApiData Timeout where
  toQueryParam = intToText . unTimeout

data Mode = Telegram | Terminal
  deriving (Show, Eq)

data Config = Config
  { cInfo :: Info,
    cInitRC :: RepeatNum,
    cToken :: Token,
    cTimeout :: Timeout,
    cMode :: Mode
  }
  deriving (Show, Eq)
