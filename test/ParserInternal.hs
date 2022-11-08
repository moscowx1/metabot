module ParserInternal (spec) where

import Config.ParserInternal
  ( ParseErr (Empty, NoParse, TooBig, TooSmall),
    between,
    exact,
    nonEmpty',
    or',
    run,
    shoudlBeMore,
    shouldBeLess,
  )
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  specNonEmpty
  specExact
  specLess
  specBetweeen
  specOr

specNonEmpty :: Spec
specNonEmpty =
  describe "func nonempty'" $ do
    it "success" $ do
      let s = "hello"
      let result = run "" nonEmpty' s
      result `shouldBe` Right (fromJust $ nonEmpty s)
    it "fail" $ do
      let varName = "variable"
      let result = run varName nonEmpty' ""
      result `shouldBe` Left (Empty varName)

specExact :: Spec
specExact =
  describe "func exact" $ do
    it "success" $ do
      let s = "asdfasgdf"
      let result = run "" (exact s) s
      result `shouldBe` Right s
    it "fail" $ do
      let varName = "kaka"
      let result = run varName (exact "as") "sa"
      result `shouldBe` Left (NoParse varName)

specLess :: Spec
specLess =
  describe "func shouldBeLess" $ do
    it "success" $ do
      let val = "a"
      let result = run "" (shouldBeLess "b") val
      result `shouldBe` Right val
    it "fail" $ do
      let varName = "aka"
      let result = run varName (shouldBeLess "a") "b"
      result `shouldBe` Left (TooBig varName)

specMore :: Spec
specMore =
  describe "func shouldBeMore" $ do
    it "success" $ do
      let val = "b"
      let result = run "" (shoudlBeMore "a") val
      result `shouldBe` Right val
    it "fail" $ do
      let varName = "baka"
      let result = run varName (shoudlBeMore "b") "a"
      result `shouldBe` Left (TooSmall varName)

specBetweeen :: Spec
specBetweeen =
  describe "func between" $ do
    it "success" $ do
      let val = "b"
      let result = run "" (between "a" "c") val
      result `shouldBe` Right val
    it "fail on too big" $ do
      let varN = "vars"
      let result = run varN (between "a" "c") "d"
      result `shouldBe` Left (TooBig varN)
    it "fail on too small" $ do
      let varN = "awqw"
      let result = run varN (between "c" "d") "a"
      result `shouldBe` Left (TooSmall varN)

specOr :: Spec
specOr =
  describe "func or'" $ do
    it "success first" $ do
      let res = "hi"
      let p1 = exact res res
      let p2 = exact "hu" "hu"
      let pOr = runExcept $ runReaderT (p1 `or'` p2) ""
      pOr `shouldBe` Right res
    it "success second" $ do
      let res = "hu"
      let p1 = exact "hi" "ho"
      let p2 = exact res res
      let pOr = runExcept $ runReaderT (p1 `or'` p2) ""
      pOr `shouldBe` Right res
    it "failed both" $ do
      let p1 = exact "hi" "ho"
      let p2 = exact "hu" "hi"
      let varN = "varka"
      let pOr = runExcept $ runReaderT (p1 `or'` p2) varN
      pOr `shouldBe` Left (NoParse varN)