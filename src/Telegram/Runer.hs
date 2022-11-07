{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Runer (getter, mapper) where

import Config.Data (Config (..), RepeatNum)
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift))
import Control.Monad.RWS (asks, modify)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Handle (Handle)
import Servant.Client (ClientError, ClientM, runClientM)
import Telegram.Api (getUpdates, sendMessage)
import Telegram.Data (Chat (..), Message (..), Update (..), Updates (result))
import Telegram.State (TelegramSt (..))

type TelegramHandle = Handle TelegramSt

returnReq :: IO (Either ClientError a) -> TelegramHandle a
returnReq = lift . lift . ExceptT

runReq :: ClientM b -> TelegramHandle b
runReq m = do
  TelegramSt {tEnv} <- lift get
  returnReq $ runClientM m tEnv

sendMsg :: Int -> String -> TelegramHandle ()
sendMsg id' msg = do
  Config {cToken} <- ask
  runReq (sendMessage cToken id' msg) >> pure ()

updateOffset :: Update -> TelegramHandle ()
updateOffset up = do
  let uId = updateId up
  lift $ modify (\s -> s {tOffset = uId + 1})

getRn :: Update -> TelegramHandle RepeatNum
getRn up = do
  TelegramSt {tIdToRN} <- get
  let id' = chatId $ messageChat $ updateMessage up
  case lookup id' tIdToRN of
    Just x -> pure x
    Nothing -> do
      k <- asks cInitRC
      lift $ modify (\s -> s {tIdToRN = (id', k) : tIdToRN})
      pure k

type Sender = String -> TelegramHandle ()

type OnMessageSend = TelegramHandle ()

mapper :: Update -> TelegramHandle (String, Sender, RepeatNum, OnMessageSend)
mapper up = do
  let id' = chatId $ messageChat $ updateMessage up
  rn <- getRn up
  pure (messageText $ updateMessage up, sendMsg id', rn, updateOffset up)

getter :: TelegramHandle [Update]
getter = do
  Config {cToken, cTimeout} <- ask
  TelegramSt {tOffset} <- lift get
  runReq
    ( result
        <$> getUpdates
          cToken
          cTimeout
          (Just tOffset)
    )