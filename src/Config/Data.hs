{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.Data
  ( Info,
    infoEither,
    Token,
    tokenEither,
    Timeout,
    timeoutEither,
    RepeatNum,
    repeatNumEither,
    ParseErr,
    Parser,
  )
where

import Control.Monad.Except (Except, runExcept, throwError, when)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Text.Read (readMaybe)

type VarName = String

data ParseErr
  = Empty VarName
  | TooBig VarName
  | TooSmall VarName
  | NoParse VarName

type Parser' = ReaderT VarName (Except ParseErr)

type Parser a = String -> Either ParseErr a

throw' :: (VarName -> ParseErr) -> Parser' a
throw' ctor = ask >>= throwError . ctor

nonEmpty' :: String -> Parser' (NonEmpty Char)
nonEmpty' v = maybe (throw' Empty) pure (nonEmpty v)

tryRead :: (Read a) => NonEmpty Char -> Parser' a
tryRead x = maybe (throw' NoParse) pure (readMaybe $ toList x)

between :: Int -> Int -> Int -> Parser' Int
between min' max' v = do
  when (min' > v) (throw' TooSmall)
  when (v > max') (throw' TooBig)
  pure v

newtype RepeatNum = RepeatNum {unRepeatNum :: Int}
  deriving (Show, Eq)

repeatNum :: String -> Parser' RepeatNum
repeatNum i =
  nonEmpty' i
    >>= tryRead
    >>= between 1 5
    <&> RepeatNum

run ::
  VarName ->
  (String -> Parser' a) ->
  Parser a
run s p = runExcept . runReaderT (p s)

repeatNumEither :: Parser RepeatNum
repeatNumEither = run "repeat number" repeatNum

newtype Info = Info {unInfo :: NonEmpty Char}
  deriving (Show, Eq)

info :: String -> Parser' Info
info v = nonEmpty' v <&> Info

infoEither :: Parser Info
infoEither = run "info" info

newtype Token = Token {unToken :: NonEmpty Char}
  deriving (Show, Eq)

token :: String -> Parser' Token
token v = nonEmpty' v <&> Token

tokenEither :: Parser Token
tokenEither = run "token" token

newtype Timeout = Timeout {unTimeout :: Int}
  deriving (Show, Eq)

timeout :: String -> Parser' Timeout
timeout v =
  nonEmpty' v
    >>= tryRead
    >>= between 10 1000
    <&> Timeout

timeoutEither :: Parser Timeout
timeoutEither = run "timeout" timeout