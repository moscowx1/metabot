module Config.ParserInternal
  ( ParseErr (..),
    Parser,
    Parser',
    between,
    exact,
    nonEmpty',
    or',
    run,
    tryRead,
    shouldBeLess,
    shoudlBeMore,
  )
where

import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExcept)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Text.Read (readMaybe)

type VarName = String

type Parser a = String -> Either ParseErr a

data ParseErr
  = Empty VarName
  | TooBig VarName
  | TooSmall VarName
  | NoParse VarName
  deriving (Show, Eq)

type Parser' = ReaderT VarName (ExceptT ParseErr Identity)

throw' :: (VarName -> ParseErr) -> Parser' a
throw' ctor = ask >>= throwError . ctor

nonEmpty' :: String -> Parser' (NonEmpty Char)
nonEmpty' v = maybe (throw' Empty) pure (nonEmpty v)

tryRead :: (Read a) => NonEmpty Char -> Parser' a
tryRead x = maybe (throw' NoParse) pure (readMaybe $ toList x)

exact :: String -> String -> Parser' String
exact s i =
  if s == i
    then pure i
    else throw' NoParse

shouldBeLess :: Ord a => a -> a -> Parser' a
shouldBeLess l v =
  if v > l
    then throw' TooBig
    else pure v

shoudlBeMore :: Ord a => a -> a -> Parser' a
shoudlBeMore m v =
  if m > v
    then throw' TooSmall
    else pure v

between :: Ord a => a -> a -> a -> Parser' a
between min' max' v = shouldBeLess max' v >> shoudlBeMore min' v

or' :: Parser' a -> Parser' a -> Parser' a
or' p1 p2 = p1 `catchError` const p2


run ::
  VarName ->
  (String -> Parser' a) ->
  Parser a
run v p s = runExcept $ runReaderT (p s) v