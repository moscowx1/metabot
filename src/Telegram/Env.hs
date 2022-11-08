{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Env (telegramEnv, TelegramEnv (..)) where

import Config.Data
  ( Config (Config, cInfo, cInitRC, cTimeout, cToken),
    HasInfo (getInfo),
    Info (unInfo),
    RepeatNum,
    Timeout,
    Token,
  )
import Data.List.NonEmpty (toList)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (Https), mkClientEnv)

url :: BaseUrl
url = BaseUrl Https "api.telegram.org" 443 ""

env :: IO ClientEnv
env = do
  man <- newManager tlsManagerSettings
  pure $ mkClientEnv man url

telegramEnv :: Config -> IO TelegramEnv
telegramEnv
  Config
    { cInitRC,
      cTimeout,
      cToken,
      cInfo
    } = do
    e <- env
    pure $ TelegramEnv e cTimeout cInitRC cToken cInfo

data TelegramEnv = TelegramEnv
  { tEnv :: ClientEnv,
    tTimeout :: Timeout,
    tInitRN :: RepeatNum,
    tToken :: Token,
    tInfo :: Info
  }

instance HasInfo TelegramEnv where
  getInfo = toList . unInfo . tInfo