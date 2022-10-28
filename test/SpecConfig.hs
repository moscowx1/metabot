{-# LANGUAGE OverloadedStrings #-}

module SpecConfig(spec) where

import Config.Data (infoEither, repeatNumEither, tokenEither, timeoutEither)
import Data.Either (isLeft)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec
spec = do
  specRepeatNum
  specInfo
  specToken
  specTimeout

specRepeatNum :: Spec
specRepeatNum =
  describe "parsing repeat number" $ do
    it "success" $ do
      repeatNumEither "3" `shouldBe` pure (unsafeCoerce 3)

    it "fail (out of range)" $ do
      repeatNumEither "0" `shouldSatisfy` isLeft

    it "fail (empty string)" $ do
      repeatNumEither "" `shouldSatisfy` isLeft

specInfo :: Spec
specInfo =
  describe "parsing info" $ do
    it "success" $ do
      let k = unsafeCoerce $ fromJust $ nonEmpty "hihi"
      infoEither "hihi" `shouldBe` pure k

    it "fail (empty string)" $ do
      infoEither "" `shouldSatisfy` isLeft

specToken :: Spec
specToken =
  describe "parsing token" $ do
    it "success" $ do
      let k = unsafeCoerce $ fromJust $ nonEmpty "token"
      tokenEither "token" `shouldBe` pure k

    it "fail (empty string)" $ do
      tokenEither "" `shouldSatisfy` isLeft

specTimeout :: Spec
specTimeout =
  describe "parsing timeout" $ do
    it "success" $ do
      timeoutEither "1000" `shouldBe` pure (unsafeCoerce 1000)

    it "fail (empty string)" $ do
      timeoutEither "" `shouldSatisfy` isLeft

    it "fail (out of range)" $ do
      timeoutEither "222222222222222" `shouldSatisfy` isLeft
