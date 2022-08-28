{-# LANGUAGE OverloadedStrings #-}

module Bot.DataSpec (spec) where

import Data.Either (isLeft)
import Data.Internal
  ( readHelpMessage,
    readInitOffset,
    readRepeatCount,
    readRepeatMessage,
    readTimeout,
    readToken,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "testing bot data reading" $ do
    it "should read repeat count" $ do
      readRepeatCount "3" `shouldBe` pure 3

    it "should return left on invalid repeat count" $ do
      readRepeatCount "lnasd3" `shouldSatisfy` isLeft

    it "should return left on too big repeat count" $ do
      readRepeatCount "6" `shouldSatisfy` isLeft

    it "should return left on too small repeat count" $ do
      readRepeatCount "0" `shouldSatisfy` isLeft

    it "should read help message" $ do
      readHelpMessage "helzxc3" `shouldBe` Right "helzxc3"

    it "should return left on empty help message" $ do
      readHelpMessage "" `shouldSatisfy` isLeft

    it "should read repeat message" $ do
      readRepeatMessage "asdxz3" `shouldBe` Right "asdxz3"

    it "should return left on empty repeat message" $ do
      readRepeatMessage "" `shouldSatisfy` isLeft

    it "should read token" $ do
      readToken "asdfawdf" `shouldBe` pure "asdfawdf"

    it "should return left on empty token" $ do
      readToken "" `shouldSatisfy` isLeft

    it "should read timeout" $ do
      readTimeout "333" `shouldBe` pure 333

    it "should return left on small timeout" $ do
      readTimeout "-123" `shouldSatisfy` isLeft

    it "should return left on too big timeout" $ do
      readTimeout "123123123123123" `shouldSatisfy` isLeft

    it "should read empty offset" $ do
      readInitOffset "" `shouldBe` pure Nothing

    it "should read non empty offset" $ do
      readInitOffset "123123" `shouldBe` (pure . pure) 123123

    it "should return left on negative offset" $ do
      readInitOffset "-123" `shouldSatisfy` isLeft
