{-# LANGUAGE OverloadedStrings #-}

module ResponseSpec (spec) where

import Response
  ( Chat (Chat),
    From (From),
    Message (Message),
    SendMessageResponse (SendMessageResponse),
    Update (Update),
    UpdateResponse (UpdateResponse),
  )
import Data.Aeson (eitherDecodeStrict)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "parsing telegram response" $ do
    it "should parse chat" $ do
      let json =
            "{\"id\":12,\
            \\"first_name\":\"kelvin\",\
            \\"username\":\"kelvinator\",\
            \\"type\":\"private\"}"
      let expected = Chat 12 "kelvin" "kelvinator" "private"

      let actual = eitherDecodeStrict json
      actual `shouldBe` pure expected

    it "should parse from" $ do
      let json =
            "{\"id\":4,\
            \\"is_bot\":false,\
            \\"first_name\":\"dave\",\
            \\"username\":\"dave_nagibator\",\
            \\"language_code\":\"en\"}"

      let expected = From 4 False "dave" "dave_nagibator" "en"

      let actual = eitherDecodeStrict json
      actual `shouldBe` pure expected

    it "should parse message" $ do
      let json =
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

      let actual = eitherDecodeStrict json
      actual `shouldBe` pure expected

    it "should parse update" $ do
      let json =
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

      let actual = eitherDecodeStrict json
      actual `shouldBe` pure expected

    it "should parse update response" $ do
      let json =
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

      let actual = eitherDecodeStrict json
      actual `shouldBe` pure expected

    it "should parse send message response" $ do
      let json =
            "{\"ok\":false,\
            \\"error_code\":404,\
            \\"description\":\"Not Found\"}"
      let expected = SendMessageResponse False (Just 404) (Just "Not Found")

      let actual = eitherDecodeStrict json

      actual `shouldBe` pure expected

    it "should parse send message response without description" $ do
      let json =
            "{\"ok\":false,\
            \\"error_code\":404}"
      let expected = SendMessageResponse False (Just 404) Nothing

      let actual = eitherDecodeStrict json

      actual `shouldBe` pure expected
