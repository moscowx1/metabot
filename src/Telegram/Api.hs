{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.Api (sendMessage, getUpdates) where

import Config.Data (Timeout, Token)
import Data.Proxy (Proxy (Proxy))
import Servant.API
  ( Capture,
    Get,
    JSON,
    QueryParam,
    QueryParam',
    Required,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Client (ClientM, client)
import Telegram.Data (MessageResponse, Updates)

type WithToken = Capture "token" Token

type Api =
  WithToken :> SendMessage
    :<|> WithToken :> GetUpdates

type SendMessage =
  "sendMessage"
    :> QueryParam' '[Required] "chat_id" Int
    :> QueryParam' '[Required] "text" String
    :> Get '[JSON] MessageResponse

type GetUpdates =
  "getUpdates"
    :> QueryParam' '[Required] "timeout" Timeout
    :> QueryParam "offset" Int
    :> Get '[JSON] Updates

api :: Proxy Api
api = Proxy

getUpdates ::
  Token ->
  Timeout ->
  Maybe Int ->
  ClientM Updates
sendMessage ::
  Token ->
  Int ->
  String ->
  ClientM MessageResponse
(sendMessage :<|> getUpdates) = client api