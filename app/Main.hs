module Main (main) where

import Config.Core (readConfigEither)
import Control.Monad (forever)
import Core (runer)
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
  runHandle
    config
    (stateS e)
    (forever runer)
    >> pure ()
