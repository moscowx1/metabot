{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module State where

import Config.Core (Config (cInitRC))
import Config.Data (RepeatNum)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (MonadState (get), StateT, modify)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Servant.Client (ClientEnv, ClientError)

type Handle = ReaderT Config (StateT StateS (ExceptT ClientError IO)) -- Move??

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