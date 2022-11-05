{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebRuner where

import Api (getUpdates, sendMessage)
import ApiData
  ( Chat (chatId),
    Message (messageChat, messageText),
    Update (updateId, updateMessage),
    Updates (result),
  )
import Config.Data (Config (..), unInfo)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift))
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify)
import Data.List.NonEmpty (toList)
import Handle (ChatId, Handle, StateS (StateS, sEnv, sOffset), getRN)
import Servant.Client (ClientError, ClientM, runClientM)

idMsg :: Update -> (Int, String)
idMsg upd = (chat upd, msg upd)
  where
    chat = chatId . messageChat . updateMessage
    msg = messageText . updateMessage

returnReq :: IO (Either ClientError a) -> Handle a
returnReq = lift . lift . ExceptT

runReq :: ClientM b -> Handle b
runReq m = do
  StateS {sEnv} <- lift get
  returnReq $ runClientM m sEnv

sendMessage' :: ChatId -> String -> Handle ()
sendMessage' chatId msg = do
  Config {cToken} <- ask
  runReq (sendMessage cToken chatId msg) >> pure ()

handleMessage :: (ChatId, String) -> Handle ()
handleMessage (chatId, msg) = do
  case msg of
    "/help" -> do
      info <- asks (toList . unInfo . cInfo)
      sendMessage' chatId info
    _ -> do
      c <- getRN chatId
      forM_ [1 .. c] (const $ sendMessage' chatId msg)

getUpdates' :: Handle [Update]
getUpdates' = do
  Config {cToken, cTimeout} <- ask
  StateS {sOffset} <- lift get
  res <-
    runReq
      ( getUpdates
          cToken
          cTimeout
          (Just sOffset)
      )
  case result res of
    [] -> getUpdates'
    xs -> pure xs

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