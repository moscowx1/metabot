{-# LANGUAGE OverloadedStrings #-}

module Tmp () where

import Bot.Config (lookupPort, lookupBotSection)
import Data.Either.Combinators (mapLeft)
import Data.Ini (parseIni)
import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "reading config file test" $ do
    it "should correct read bot section" $ do
      5 `shouldBe` 5