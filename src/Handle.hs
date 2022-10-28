{-# LANGUAGE NamedFieldPuns #-}

module Handle where

import Config.Core (Config (cInitRC))
import Config.Data (RepeatNum)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get), StateT (runStateT), modify)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Servant.Client (ClientEnv, ClientError)

type ChatId = Int

type Offset = Int

addDefRN :: ChatId -> Handle RepeatNum
addDefRN chatId = do
  defRN <- asks cInitRC
  modify (\s@StateS {sIdToRN} -> s {sIdToRN = (chatId, defRN) : sIdToRN})
  pure defRN

getOrAddRN :: ChatId -> Handle RepeatNum
getOrAddRN chatId = do
  StateS {sIdToRN} <- get
  fromMaybe (addDefRN chatId) (lookup chatId sIdToRN <&> pure)

data StateS = StateS
  { sOffset :: Offset,
    sIdToRN :: [(ChatId, RepeatNum)],
    sEnv :: ClientEnv
  }

type Handle = ReaderT Config (StateT StateS (ExceptT ClientError IO)) -- Move??

runHandle ::
  Config ->
  StateS ->
  Handle a ->
  IO (Either ClientError (a, StateS))
runHandle e st m = runExceptT $ runStateT (runReaderT m e) st