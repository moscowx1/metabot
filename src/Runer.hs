{-# LANGUAGE FlexibleContexts #-}

module Runer (run) where

import Config.Data (Config (..), Mode (..), RepeatNum, unInfo)
import Control.Monad (forever, (>=>))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT))
import Data.List.NonEmpty (toList)
import Handle (Handle)
import qualified Telegram.Runer as Telegram
import Telegram.State (TelegramSt, telegramState)
import qualified Terminal.Runer as Terminal

handleMsg :: String -> (String -> Handle st ()) -> RepeatNum -> Handle st ()
handleMsg msg sender rn = do
  case msg of
    "/help" -> do
      info <- asks (toList . unInfo . cInfo)
      sender info
    _ -> do
      mapM_ (const (sender msg)) [1 .. rn]

handle :: (String, String -> Handle st (), RepeatNum, Handle st ()) -> Handle st ()
handle (msg, sender, rn, onSent) = do
  handleMsg msg sender rn >> onSent

runer :: Handle st [m] -> (m -> Handle st (String, String -> Handle st (), RepeatNum, Handle st ())) -> Handle st ()
runer getter mapper = do
  ups <- getter
  mapM_ (mapper >=> handle) ups

telegramRuner :: Handle TelegramSt ()
telegramRuner = runer Telegram.getter Telegram.mapper

terminalRuner :: Handle RepeatNum ()
terminalRuner = runer Terminal.getter Terminal.mapper

runExcept' :: Monad m => ExceptT e m a -> m ()
runExcept' (ExceptT t) = t >> pure ()

run :: Config -> IO ()
run config = do
  let c = cMode config
  case c of
    Terminal -> do
      let st = cInitRC config
      runExcept' $
        runStateT (runReaderT (forever terminalRuner) config) st
    Telegram -> do
      st <- telegramState
      runExcept' $
        runStateT
          (runReaderT (forever telegramRuner) config)
          st
