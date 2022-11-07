{-# LANGUAGE FlexibleContexts #-}

module Telegram.State (TelegramSt (..), telegramState) where

import Config.Data (RepeatNum)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, Scheme (Https), mkClientEnv)
import Telegram.Data (ChatId)

type Offset = Int

url :: BaseUrl
url = BaseUrl Https "api.telegram.org" 443 ""

env :: IO ClientEnv
env = do
  man <- newManager tlsManagerSettings
  pure $ mkClientEnv man url

telegramState :: IO TelegramSt
telegramState = TelegramSt 0 [] <$> env

data TelegramSt = TelegramSt
  { tOffset :: Offset,
    tIdToRN :: [(ChatId, RepeatNum)],
    tEnv :: ClientEnv -- TODO: move to readerT?
  }