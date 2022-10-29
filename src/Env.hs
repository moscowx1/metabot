module Env (env) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, Scheme (Https), mkClientEnv)

telegramUrl :: BaseUrl
telegramUrl = BaseUrl Https "api.telegram.org" 443 ""

env :: IO ClientEnv
env = do
  man <- newManager tlsManagerSettings
  pure $ mkClientEnv man telegramUrl