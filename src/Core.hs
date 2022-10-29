{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core (runer) where

import Api (getUpdates, sendMessage)
import ApiData
  ( Chat (chatId),
    Message (messageChat, messageText),
    MessageResponse,
    Update (Update, updateId, updateMessage),
    Updates (result),
  )
import Config.Core (Config (Config, cInfo, cTimeout, cToken))
import Config.Data (unInfo)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift))
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify)
import Data.List.NonEmpty (toList)
import Debug.Trace (traceShow)
import Handle (ChatId, Handle, StateS (StateS, sEnv, sOffset), getOrAddRN)
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
  traceShow sOffset $
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

runer :: Handle ()
runer = getUpdates' >>= mapM_ handleUpdate . result
