{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core () where

import Api (getUpdates, sendMessage)
import ApiData (Chat (chatId), Message (messageChat, messageText), Update (Update, updateId, updateMessage), Updates (result))
import Control.Exception (throw)
import Control.Monad (forM_, forever)
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadTrans (lift), runExceptT)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask, asks)
import Control.Monad.Trans.State (StateT (StateT, runStateT), get, modify)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientError, Scheme (Https), mkClientEnv, runClientM)

data Config = Config
  { configToken :: String,
    configUrl :: BaseUrl,
    configHost :: String,
    configTimeout :: Int,
    configDefRP :: Int,
    configDescription :: String
  }

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
  Config {configToken} <- ask
  runReq (sendMessage configToken chatId msg)

addRp :: Int -> Handle Int
addRp id = asks configDefRP >>= \c -> lift (modify (mf c)) >> pure c
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
      info <- asks configDescription
      sendMessage' chatId info
      pure ()
    _ -> do
      c <- getOrAddRp chatId
      forM_ [1 .. c] (const $ sendMessage' chatId msg)

getUpdates' :: Handle Updates
getUpdates' = do
  Config {configToken, configTimeout} <- ask
  State {stateOffset} <- lift get
  runReq
    ( getUpdates
        configToken
        configTimeout
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
    stateIdToCount :: [(Int, Int)],
    stateEnv :: ClientEnv
  }

getState = State 0 []

getConf :: Config
getConf =
  Config
    { configToken = token,
      configUrl = BaseUrl Https host 443 "",
      configHost = host,
      configTimeout = 1000,
      configDefRP = 2,
      configDescription = "echo bot"
    }
  where
    host = "api.telegram.org"
    token = "bot5758182558:AAHvdgfhit1CFDBWV5Paegja8m3n5RaYkDw"

runHandle e st m = runExceptT $ runStateT (runReaderT m e) st

-- TODO:: next step -> add right offset (diff var)

main = do
  manager' <- newManager tlsManagerSettings
  let conf = getConf
  let env = mkClientEnv manager' (configUrl conf)
  let st = getState env
  forever $ runHandle conf st run''