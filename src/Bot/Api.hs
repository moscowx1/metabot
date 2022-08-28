{-# LANGUAGE OverloadedStrings #-}

module Bot.Api (sendMessage, getUpdates) where

import Bot.Response (SendMessageResponse, UpdateResponse)
import Control.Exception (try)
import qualified Data as D
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.ByteString.UTF8 as BS8
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (HttpException, Request)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Simple as HS

data Method = POST | GET
  deriving (Eq, Show)

toBS8 :: Show a => a -> BS8.ByteString
toBS8 = BS8.fromString . show

requestBase :: Request
requestBase =
  HS.setRequestPort 443 $
    HS.setRequestHost "api.telegram.org" $
      HS.setRequestSecure True HS.defaultRequest

pathBase :: D.Handle -> BS8.ByteString
pathBase h = "/bot" <> (encodeUtf8 . D.sToken . D.hConfig) h

updateQuery :: D.Handle -> HS.Query
updateQuery h =
  [ ("timeout", toBS8 <$> Just timeout),
    ("offset", toBS8 <$> Just offset)
  ]
  where
    offset = (D.ssOffset . D.hState) h
    timeout = (D.sTimeout . D.hConfig) h

updatesRequest :: D.Handle -> Request
updatesRequest h =
  HS.setRequestPath path $
    HC.setQueryString (updateQuery h) requestBase
  where
    path = pathBase h <> "/getUpdates"

sendMessageQuery :: D.ChatId -> String -> HS.Query
sendMessageQuery chatId msg =
  [ ("text", toBS8 <$> Just msg),
    ("chat_id", toBS8 <$> Just chatId)
  ]

sendMessageRequest :: D.Handle -> D.ChatId -> String -> Request
sendMessageRequest handle chatId message =
  HS.setRequestPath path $
    HC.setQueryString query requestBase
  where
    query = sendMessageQuery chatId message
    path = pathBase handle <> "/sendMessage"

isSuccess :: Int -> Bool
isSuccess code = (code >= 200) && (code < 300)

handleResponse :: HS.Response BLC8.ByteString -> Either String BLC8.ByteString
handleResponse response =
  if isSuccess statusCode
    then Right body
    else
      Left $
        mconcat
          [ "not successful status code: ",
            show statusCode,
            ". Get body: ",
            show body
          ]
  where
    statusCode = HS.getResponseStatusCode response
    body = HS.getResponseBody response

runRequest :: Request -> IO (Either String BLC8.ByteString)
runRequest req = do
  res <- try $ HS.httpLBS req
  pure $ case res of
    Left err -> Left $ show (err :: HttpException)
    Right response -> handleResponse response

getResponse :: FromJSON a => Request -> IO (Either String a)
getResponse req = do
  res <- runRequest req
  pure $ case res of
    Left err -> Left err
    Right body -> eitherDecode body

getUpdates :: D.Handle -> IO (Either String UpdateResponse)
getUpdates h = getResponse (updatesRequest h)

sendMessage ::
  D.Handle ->
  D.ChatId ->
  String ->
  IO (Either String SendMessageResponse)
sendMessage handle chatId msg = getResponse req
  where
    req = sendMessageRequest handle chatId msg
