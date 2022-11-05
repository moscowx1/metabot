module Main (main) where

import Config.Data (Config (cMode))
import Config.Reader (readConfigEither)
import Control.Monad (forever)
import Core (getRuner)
import Data.Ini (readIniFile)
import Env (env)
import Handle (runHandle, stateS)

lt :: (Show a) => IO (Either a b) -> IO b
lt m = do
  v <- m
  case v of
    Left x -> error $ show x
    Right b -> pure b

main :: IO ()
main = do
  ini <- lt $ readIniFile "config.ini"
  config <- lt $ pure $ readConfigEither ini
  e <- env
  let r = getRuner (cMode config)
  runHandle config (stateS e) (forever r) >> pure ()
