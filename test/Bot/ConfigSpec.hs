{-# LANGUAGE OverloadedStrings #-}

module Bot.ConfigSpec (spec) where

import Bot.Config
  ( lookupHelpMessage,
    lookupPort,
    lookupRepeatCount,
    lookupRepeatMessage,
  )
import Data.Either.Combinators (isLeft, mapLeft)
import Data.Ini (parseIni)
import Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "reading config file test" $ do
    it "should read port" $ do
      let ini = mapLeft T.pack $ parseIni "[Server]\nPort: 6667\n"
      let port = ini >>= lookupPort
      port `shouldBe` pure 6667

    it "should return left while reading ini without server section" $ do
      let ini = mapLeft T.pack $ parseIni "[Sever]\nPort: 6667\n"
      let port = ini >>= lookupPort
      port `shouldSatisfy` isLeft

    it "should return left while reading ini without port key" $ do
      let ini = mapLeft T.pack $ parseIni "[Server]\nasdgf: 6667\n"
      let port = ini >>= lookupPort
      port `shouldSatisfy` isLeft

    it "should read helpMessage" $ do
      let ini = mapLeft T.pack $ parseIni "[Bot]\nHelpMessage: baba\n"
      let helpMessage = ini >>= lookupHelpMessage
      helpMessage `shouldBe` pure "baba"

    it "should return left while reading ini without helpMessage key" $ do
      let ini = mapLeft T.pack $ parseIni "[Bot]\nasd: baba\n"
      let helpMessage = ini >>= lookupHelpMessage
      helpMessage `shouldSatisfy` isLeft

    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft T.pack $ parseIni "[Bobo]\nHelpMessage: baba\n"
      let helpMessage = ini >>= lookupHelpMessage
      helpMessage `shouldSatisfy` isLeft

    it "should read repeatMessage" $ do
      let ini = mapLeft T.pack $ parseIni "[Bot]\nRepeatMessage: rep\n"
      let repeatMessage = ini >>= lookupRepeatMessage
      repeatMessage `shouldBe` pure "rep"

    it "should return left while reading ini without repeat message key" $ do
      let ini = mapLeft T.pack $ parseIni "[Bot]\nasd: baba\n"
      let repeatMessage = ini >>= lookupRepeatMessage
      repeatMessage `shouldSatisfy` isLeft
    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft T.pack $ parseIni "[Bobo]\nRepeatMessage: baba\n"
      let repeatMessage = ini >>= lookupRepeatMessage
      repeatMessage `shouldSatisfy` isLeft

    it "should read repeatCount" $ do
      let ini = mapLeft T.pack $ parseIni "[Bot]\nRepeatCount: 3\n"
      let repeatCount = ini >>= lookupRepeatCount
      repeatCount `shouldBe` pure 3

    it "should return left while reading ini without repeat count key" $ do
      let ini = mapLeft T.pack $ parseIni "[Bot]\nasd: asdfg\n"
      let repeatCount = ini >>= lookupRepeatCount 
      repeatCount `shouldSatisfy` isLeft

    it "should return left while reading ini without bot section" $ do
      let ini = mapLeft T.pack $ parseIni "[Bobo]\nRepeatCount: 123\n"
      let repeatCount = ini >>= lookupRepeatCount
      repeatCount `shouldSatisfy` isLeft
