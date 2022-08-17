{-# LANGUAGE OverloadedStrings #-}

module Bot.DataSpec (spec) where

import Bot.Data (readHelpMessage, readPort, readRepeatCount, readRepeatMessage)
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "testing bot data reading" $ do
    it "should read port" $ do
      readPort "3030" `shouldBe` pure 3030

    it "should return left on invalid port" $ do
      readPort "as123" `shouldSatisfy` isLeft

    it "should return left on too big port" $ do
      readPort "1000000" `shouldSatisfy` isLeft

    it "should return left on too small port" $ do
      readPort "0" `shouldSatisfy` isLeft

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
