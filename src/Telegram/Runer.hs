{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Runer (sender, getter) where

import Config.Data (Config (..))
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify)
import Handle (Handle, StateS (..))
import Servant.Client (ClientError, ClientM, runClientM)
import Telegram.Api (getUpdates, sendMessage)
import Telegram.Data (Chat (..), Message (..), Update (..), Updates (result))

returnReq :: IO (Either ClientError a) -> Handle a
returnReq = lift . lift . ExceptT

runReq :: ClientM b -> Handle b
runReq m = do
  StateS {sEnv} <- lift get
  returnReq $ runClientM m sEnv

sender :: Update -> Handle ()
sender up = do
  Config {cToken} <- ask
  let id' = chatId $ messageChat $ updateMessage up
  let msg = messageText $ updateMessage up
  let uId = updateId up
  _ <- runReq (sendMessage cToken id' msg)
  lift $ modify (\s -> s {sOffset = uId + 1})

getter :: Handle [Update]
getter = do
  Config {cToken, cTimeout} <- ask
  StateS {sOffset} <- lift get
  runReq
    ( result
        <$> getUpdates
          cToken
          cTimeout
          (Just sOffset)
    )