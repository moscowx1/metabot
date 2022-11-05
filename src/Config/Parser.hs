module Config.Parser
  ( timeoutEither,
    modeEither,
    tokenEither,
    infoEither,
    repeatNumEither,
    modifyErr,
  )
where

import Config.Data
  ( Info (..),
    Mode (..),
    RepeatNum (..),
    Timeout (..),
    Token (..),
  )
import Config.ParserInternal
  ( ParseErr,
    Parser,
    Parser',
    between,
    exact,
    nonEmpty',
    or',
    run,
    tryRead,
  )
import Data.Bifunctor (first)
import Data.Functor ((<&>))

modifyErr :: Parser a -> (ParseErr -> b) -> (String -> Either b a)
modifyErr p f s = first f (p s)

timeout :: String -> Parser' Timeout
timeout v =
  nonEmpty' v
    >>= tryRead
    >>= between 10 1000
    <&> Timeout

timeoutEither :: Parser Timeout
timeoutEither = run "timeout" timeout

telegram :: String -> Parser' Mode
telegram s = exact "telegram" s >> pure Telegram

terminal :: String -> Parser' Mode
terminal s = exact "terminal" s >> pure Terminal

mode :: String -> Parser' Mode
mode s = terminal s `or'` telegram s

modeEither :: Parser Mode
modeEither = run "mode" mode

token :: String -> Parser' Token
token v = nonEmpty' v <&> Token

tokenEither :: Parser Token
tokenEither = run "token" token

info :: String -> Parser' Info
info v = nonEmpty' v <&> Info

infoEither :: Parser Info
infoEither = run "info" info

repeatNum :: String -> Parser' RepeatNum
repeatNum i =
  nonEmpty' i
    >>= tryRead
    >>= between 1 5
    <&> RepeatNum

repeatNumEither :: Parser RepeatNum
repeatNumEither = run "repeat number" repeatNum