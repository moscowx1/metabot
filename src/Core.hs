{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core (Handle) where

import Api (getUpdates, sendMessage)
import ApiData (Chat (chatId), Message (messageChat, messageText), MessageResponse, Update (Update, updateId, updateMessage), Updates (result))
import Config.Core (Config (Config, cInfo, cTimeout, cToken))
import Config.Data (unInfo)
import Control.Monad (forM_, forever)
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift), runExceptT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans.State (StateT (runStateT), get, modify)
import Data.List.NonEmpty (toList)
import Servant.Client (ClientError, ClientM, runClientM)
import State (ChatId, Handle, StateS (StateS, sEnv, sOffset), getOrAddRN)

idMsg :: Update -> (Int, String)
idMsg upd = (chat upd, msg upd)
  where
    chat = chatId . messageChat . updateMessage
    msg = messageText . updateMessage

returnReq :: IO (Either ClientError a) -> Handle a
returnReq = lift . lift . ExceptT

runReq ::
  ClientM b ->
  ReaderT Config (StateT StateS (ExceptT ClientError IO)) b
runReq m = do
  StateS {sEnv} <- lift get
  returnReq $ runClientM m sEnv

sendMessage' :: ChatId -> String -> Handle MessageResponse
sendMessage' chatId msg = do
  Config {cToken} <- ask
  runReq (sendMessage cToken chatId msg)

handleMessage :: (ChatId, String) -> Handle ()
handleMessage (chatId, msg) = do
  case msg of
    "/help" -> do
      info <- asks (toList . unInfo . cInfo)
      sendMessage' chatId info >> pure ()
    _ -> do
      c <- getOrAddRN chatId
      forM_ [1 .. c] (const $ sendMessage' chatId msg)

getUpdates' :: Handle Updates
getUpdates' = do
  Config {cToken, cTimeout} <- ask
  StateS {sOffset} <- lift get
  runReq
    ( getUpdates
        cToken
        cTimeout
        (Just sOffset)
    )

handleUpdate :: Update -> Handle ()
handleUpdate u@Update {updateId} = handleMessage (idMsg u) >> mod'
  where
    mod' = lift $ modify (\s -> s {sOffset = updateId + 1})

run' :: Handle ()
run' = do
  ups <- getUpdates'
  forM_ (result ups) handleUpdate

run'' :: Handle ()
run'' = forever $ do
  ups <- getUpdates'
  st <- lift get
  forM_ (result ups) handleUpdate

runHandle ::
  Config ->
  StateS ->
  Handle a ->
  IO (Either ClientError (a, StateS))
runHandle e st m = runExceptT $ runStateT (runReaderT m e) st