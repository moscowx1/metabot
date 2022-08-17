module Main () where

import Bot.Config (readConfig)

main :: IO ()
main = readConfig >>= print
