module Main (main) where

import Config.Reader (readConfigEither)
import Data.Ini (readIniFile)
import Runer (run)

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
  run config
