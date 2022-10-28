{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core () where

import Api (getUpdates, sendMessage)
import ApiData (Chat (chatId), Message (messageChat, messageText), Update (Update, updateId, updateMessage), Updates (result))
import Config.Core (Config (Config, cInfo, cInitRC, cTimeout, cToken))
import Config.Data (RepeatNum, unInfo)
import Control.Exception (throw)
import Control.Monad (forM_, forever)
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadTrans (lift), runExceptT)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask, asks)
import Control.Monad.Trans.State (StateT (StateT, runStateT), get, modify)
import Data.List.NonEmpty (toList)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientError, Scheme (Https), mkClientEnv, runClientM)

idMsg :: Update -> (Int, String)
idMsg upd = (chat upd, msg upd)
  where
    chat = chatId . messageChat . updateMessage
    msg = messageText . updateMessage

returnReq :: IO (Either ClientError a) -> Handle a
returnReq = lift . lift . ExceptT

runReq m = do
  State {stateEnv} <- lift get
  returnReq $ runClientM m stateEnv

sendMessage' chatId msg = do
  Config {cToken} <- ask
  runReq (sendMessage cToken chatId msg)

addRp :: Int -> Handle RepeatNum
addRp id = asks cInitRC >>= \c -> lift (modify (mf c)) >> pure c
  where
    mf c s@State {stateIdToCount} = s {stateIdToCount = (id, c) : stateIdToCount}

--lift modify (\s@(State {stateIdToCount}) -> s {stateIdToCount = (id, rp) : stateIdToCount})

getOrAddRp id = do
  State {stateIdToCount} <- lift get
  case lookup id stateIdToCount of
    Just x -> pure x
    Nothing -> addRp id

--TODO add some det
handleMessage (chatId, msg) = do
  case msg of
    "/help" -> do
      info <- asks (toList . unInfo . cInfo)
      sendMessage' chatId info
      pure ()
    _ -> do
      c <- getOrAddRp chatId
      forM_ [1 .. c] (const $ sendMessage' chatId msg)

getUpdates' :: Handle Updates
getUpdates' = do
  Config {cToken, cTimeout} <- ask
  State {stateOffset} <- lift get
  runReq
    ( getUpdates
        cToken
        cTimeout
        (Just stateOffset)
    )

handleUpdate u@Update {updateId} = handleMessage (idMsg u) >> mod'
  where
    mod' = lift $ modify (\s -> s {stateOffset = updateId + 1})

run' :: Handle ()
run' = do
  ups <- getUpdates'
  forM_ (result ups) handleUpdate

run'' = forever $ do
  ups <- getUpdates'
  st <- lift get
  forM_ (result ups) handleUpdate

type Handle = ReaderT Config (StateT State (ExceptT ClientError IO))

data State = State
  { stateOffset :: Int,
    stateIdToCount :: [(Int, RepeatNum)],
    stateEnv :: ClientEnv
  }

getState = State 0 []

runHandle e st m = runExceptT $ runStateT (runReaderT m e) st