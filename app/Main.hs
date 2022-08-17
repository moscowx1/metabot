module Main (main) where

import Bot.Config (readConfig)

main :: IO ()
main = readConfig >>= print
