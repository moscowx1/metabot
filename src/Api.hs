{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (sendMessage, getUpdates) where

import ApiData (MessageResponse, Updates)
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

type WithToken = Capture "token" String

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
    :> QueryParam' '[Required] "timeout" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Updates

api :: Proxy Api
api = Proxy

getUpdates ::
  String ->
  Int ->
  Maybe Int ->
  ClientM Updates
sendMessage ::
  String ->
  Int ->
  String ->
  ClientM MessageResponse
(sendMessage :<|> getUpdates) = client api