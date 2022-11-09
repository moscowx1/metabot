{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runer (run) where

import Config.Data (Config (..), HasInfo (..), Info, Mode (..), RepeatNum)
import Control.Monad (forever, (>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT))
import Handle (Handle)
import Telegram.Env (TelegramEnv, telegramEnv)
import qualified Telegram.Runer as Telegram
import Telegram.State (TelegramSt, telegramState)
import qualified Terminal.Runer as Terminal

handleMsg :: (HasInfo e) => String -> (String -> Handle e st ()) -> RepeatNum -> Handle e st ()
handleMsg msg sender rn = do
  case msg of
    "/help" -> do
      info <- asks getInfo
      sender info
    _ -> do
      mapM_ (const (sender msg)) [1 .. rn]

handle :: HasInfo e => (String, String -> Handle e st (), RepeatNum, Handle e st ()) -> Handle e st ()
handle (msg, sender, rn, onSent) = do
  handleMsg msg sender rn >> onSent

runer :: HasInfo e => Handle e st [m] -> (m -> Handle e st (String, String -> Handle e st (), RepeatNum, Handle e st ())) -> Handle e st ()
runer getter mapper = do
  ups <- getter
  mapM_ (mapper >=> handle) ups

telegramRuner :: Handle TelegramEnv TelegramSt ()
telegramRuner = runer Telegram.getter Telegram.mapper

terminalRuner :: Handle Info RepeatNum ()
terminalRuner = runer Terminal.getter Terminal.mapper

runRuner :: Handle e st () -> e -> st -> IO ()
runRuner r e st = runExceptT (runStateT (runReaderT (forever r) e) st) >> pure ()

run :: Config -> IO ()
run config = do
  let c = cMode config
  case c of
    Terminal -> do
      let Config {cInitRC, cInfo} = config
      runRuner terminalRuner cInfo cInitRC
    Telegram -> do
      env <- telegramEnv config
      runRuner telegramRuner env telegramState
