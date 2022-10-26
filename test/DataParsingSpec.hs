{-# LANGUAGE OverloadedStrings #-}

module DataParsingSpec (spec) where

import Data.Either.Combinators (isLeft, mapLeft)
import Data.Ini (parseIni)
import Data.Internal
  ( ServerConfig (..),
    lookupHelpMessage,
    lookupRepeatCount,
    lookupRepeatMessage,
    lookupServerConfig,
    lookupTimeout,
    lookupToken,
  )
import Data.Text (pack)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "reading config file test" $ do
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

    it "should read timeout" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nTimeout: 30\n"
      let timeout = ini >>= lookupTimeout
      timeout `shouldBe` pure 30

    it "should return left while reading ini without timeout key" $ do
      let ini = mapLeft pack $ parseIni "[Bot]\nzxcvf: asdfg\n"
      let timeout = ini >>= lookupTimeout
      timeout `shouldSatisfy` isLeft

    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft pack $ parseIni "[Bobo]\nRepeatCount: 123\n"
      let timeout = ini >>= lookupTimeout
      timeout `shouldSatisfy` isLeft

    it "should read correct config" $ do
      let txt =
            "[Bot]\nHelpMessage: plsHelp\n\
            \RepeatMessage: +rep\n\
            \RepeatCount: 4\n\
            \Token: asvzxcv\n\
            \Timeout: 30\n\
            \InitOffset: 12"

      let config = mapLeft pack (parseIni txt) >>= lookupServerConfig
      let expectedConfig =
            ServerConfig
              { sHelpMessage = "plsHelp",
                sRepeatMessage = "+rep",
                sRepeatCount = 4,
                sToken = "asvzxcv",
                sTimeout = 30,
                sInitialOffset = Just 12
              }
      config `shouldBe` pure expectedConfig