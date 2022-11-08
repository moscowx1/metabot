{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Runer (getter, mapper) where

import Config.Data (RepeatNum)
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift))
import Control.Monad.RWS (asks, modify)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Handle (Handle)
import Servant.Client (ClientError, ClientM, runClientM)
import Telegram.Api (getUpdates, sendMessage)
import Telegram.Data (Chat (..), Message (..), Update (..), Updates (result))
import Telegram.Env (TelegramEnv (TelegramEnv, tEnv, tInitRN, tTimeout, tToken))
import Telegram.State (TelegramSt (..))

type TelegramHandle = Handle TelegramEnv TelegramSt

returnReq :: IO (Either ClientError a) -> TelegramHandle a
returnReq = lift . lift . ExceptT

runReq :: ClientM b -> TelegramHandle b
runReq m = do
  e <- asks tEnv
  returnReq $ runClientM m e

sendMsg :: Int -> String -> TelegramHandle ()
sendMsg id' msg = do
  token <- asks tToken
  runReq (sendMessage token id' msg) >> pure ()

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
      rn <- asks tInitRN
      lift $ modify (\s -> s {tIdToRN = (id', rn) : tIdToRN})
      pure rn

type Sender = String -> TelegramHandle ()

type OnMessageSend = TelegramHandle ()

mapper :: Update -> TelegramHandle (String, Sender, RepeatNum, OnMessageSend)
mapper up = do
  let id' = chatId $ messageChat $ updateMessage up
  rn <- getRn up
  pure (messageText $ updateMessage up, sendMsg id', rn, updateOffset up)

getter :: TelegramHandle [Update]
getter = do
  TelegramEnv {tToken, tTimeout} <- ask
  TelegramSt {tOffset} <- lift get
  runReq
    ( result
        <$> getUpdates
          tToken
          tTimeout
          (Just tOffset)
    )