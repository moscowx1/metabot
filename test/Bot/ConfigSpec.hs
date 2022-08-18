{-# LANGUAGE OverloadedStrings #-}

module Bot.ConfigSpec (spec) where

import Bot.Config
  ( ServerConfig (..),
    lookupHelpMessage,
    lookupPort,
    lookupRepeatCount,
    lookupRepeatMessage,
    lookupServerConfig,
    lookupToken,
  )
import Data.Either.Combinators (isLeft, mapLeft)
import Data.Ini (parseIni)
import Data.Text (pack)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "reading config file test" $ do
    it "should read port" $ do
      let ini = mapLeft pack $ parseIni "[Server]\nPort: 6667\n"
      let port = ini >>= lookupPort
      port `shouldBe` pure 6667

    it "should return left while reading ini without server section" $ do
      let ini = mapLeft pack $ parseIni "[Sever]\nPort: 6667\n"
      let port = ini >>= lookupPort
      port `shouldSatisfy` isLeft

    it "should return left while reading ini without port key" $ do
      let ini = mapLeft pack $ parseIni "[Server]\nasdgf: 6667\n"
      let port = ini >>= lookupPort
      port `shouldSatisfy` isLeft

    it "should read helpMessage" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nHelpMessage: baba\n"
      let helpMessage = ini >>= lookupHelpMessage
      helpMessage `shouldBe` pure "baba"

    it "should return left while reading ini without helpMessage key" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nasd: baba\n"
      let helpMessage = ini >>= lookupHelpMessage
      helpMessage `shouldSatisfy` isLeft

    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft pack $ parseIni "[Bobo]\nHelpMessage: baba\n"
      let helpMessage = ini >>= lookupHelpMessage
      helpMessage `shouldSatisfy` isLeft

    it "should read repeatMessage" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nRepeatMessage: rep\n"
      let repeatMessage = ini >>= lookupRepeatMessage
      repeatMessage `shouldBe` pure "rep"

    it "should return left while reading ini without repeat message key" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nasd: baba\n"
      let repeatMessage = ini >>= lookupRepeatMessage
      repeatMessage `shouldSatisfy` isLeft
    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft pack $ parseIni "[Bobo]\nRepeatMessage: baba\n"
      let repeatMessage = ini >>= lookupRepeatMessage
      repeatMessage `shouldSatisfy` isLeft

    it "should read repeatCount" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nRepeatCount: 3\n"
      let repeatCount = ini >>= lookupRepeatCount
      repeatCount `shouldBe` pure 3

    it "should return left while reading ini without repeat count key" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nasd: asdfg\n"
      let repeatCount = ini >>= lookupRepeatCount
      repeatCount `shouldSatisfy` isLeft

    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft pack $ parseIni "[Bobo]\nRepeatCount: 123\n"
      let repeatCount = ini >>= lookupRepeatCount
      repeatCount `shouldSatisfy` isLeft

    it "should read token" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nToken: asd\n"
      let token = ini >>= lookupToken
      token `shouldBe` pure "asd"

    it "should return left while reading ini without token key" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nasd: asdfg\n"
      let token = ini >>= lookupToken
      token `shouldSatisfy` isLeft

    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft pack $ parseIni "[Bobo]\nRepeatCount: 123\n"
      let token = ini >>= lookupToken
      token `shouldSatisfy` isLeft

    it "should read correct config" $ do
      let txt =
            "[Server]\nPort: 3000\n\
            \[Bot]\nHelpMessage: plsHelp\n\
            \RepeatMessage: +rep\n\
            \RepeatCount: 4\n\
            \Token: asvzxcv"
      let config = mapLeft pack (parseIni txt) >>= lookupServerConfig
      let expectedConfig =
            ServerConfig
              { sPort = 3000,
                sHelpMessage = "plsHelp",
                sRepeatMessage = "+rep",
                sRepeatCount = 4,
                sToken = "asvzxcv"
              }
      config `shouldBe` pure expectedConfig