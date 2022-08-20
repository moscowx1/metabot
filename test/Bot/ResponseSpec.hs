{-# LANGUAGE OverloadedStrings #-}

module Bot.ResponseSpec (spec) where

import Bot.Response
  ( Chat (Chat),
    From (From),
    Message (Message),
    Update (Update),
    UpdateResponse (UpdateResponse),
  )
import Data.Aeson (eitherDecodeStrict)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "parsing telegram response" $ do
    it "should parse chat" $ do
      let chat =
            "{\"id\":12,\
            \\"first_name\":\"kelvin\",\
            \\"username\":\"kelvinator\",\
            \\"type\":\"private\"}"
      let expected = Chat 12 "kelvin" "kelvinator" "private"

      let actual = eitherDecodeStrict chat

      actual `shouldBe` pure expected

    it "should parse from" $ do
      let from =
            "{\"id\":4,\
            \\"is_bot\":false,\
            \\"first_name\":\"dave\",\
            \\"username\":\"dave_nagibator\",\
            \\"language_code\":\"en\"}"

      let expected = From 4 False "dave" "dave_nagibator" "en"

      let actual = eitherDecodeStrict from
      actual `shouldBe` pure expected

    it "should parse message" $ do
      let msg =
            "{\"message_id\":5,\
            \\"from\":{\
            \\"id\":12,\
            \\"is_bot\":false,\
            \\"first_name\":\"kurt\",\
            \\"username\":\"kurt228\",\
            \\"language_code\":\"en\"},\
            \\"chat\":{\
            \\"id\":55,\
            \\"first_name\":\"kurt\",\
            \\"username\":\"kurt228\",\
            \\"type\":\"private\"},\
            \\"date\":1660935444,\
            \\"text\":\"hello world\"}"

      let expected =
            Message
              5
              (From 12 False "kurt" "kurt228" "en")
              (Chat 55 "kurt" "kurt228" "private")
              1660935444
              "hello world"

      let actual = eitherDecodeStrict msg
      actual `shouldBe` pure expected

    it "should parse update" $ do
      let response =
            "{\"update_id\":123,\
            \\"message\":{\"message_id\":48,\
            \\"from\":{\
            \\"id\":12,\
            \\"is_bot\":false,\
            \\"first_name\":\"kurt\",\
            \\"username\":\"kurt228\",\
            \\"language_code\":\"en\"},\
            \\"chat\":{\
            \\"id\":55,\
            \\"first_name\":\"kurt\",\
            \\"username\":\"kurt228\
            \\",\"type\":\"private\"},\
            \\"date\":1660935444,\
            \\"text\":\"end\"}}"

      let msg =
            Message
              48
              (From 12 False "kurt" "kurt228" "en")
              (Chat 55 "kurt" "kurt228" "private")
              1660935444
              "end"

      let expected = Update 123 msg

      let actual = eitherDecodeStrict response
      actual `shouldBe` pure expected

    it "should parse update response" $ do
      let response =
            "{\"ok\":true,\
            \\"result\":[\
            \{\"update_id\":123,\
            \\"message\":{\"message_id\":48,\
            \\"from\":{\
            \\"id\":12,\
            \\"is_bot\":false,\
            \\"first_name\":\"kurt\",\
            \\"username\":\"kurt228\",\
            \\"language_code\":\"en\"},\
            \\"chat\":{\
            \\"id\":55,\
            \\"first_name\":\"kurt\",\
            \\"username\":\"kurt228\
            \\",\"type\":\"private\"},\
            \\"date\":1660935444,\
            \\"text\":\"end\"}}]}"

      let msg =
            Message
              48
              (From 12 False "kurt" "kurt228" "en")
              (Chat 55 "kurt" "kurt228" "private")
              1660935444
              "end"
      let update = Update 123 msg
      let expected = UpdateResponse True [update]

      let actual = eitherDecodeStrict response
      actual `shouldBe` pure expected
