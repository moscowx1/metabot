{-# LANGUAGE OverloadedStrings #-}

module Bot.Response
  ( UpdateResponse (..),
    Chat (..),
    From (..),
    Message (..),
    Update (..),
    SendMessageResponse (..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (Object), (.:))
import Data.Aeson.Types (parseJSON)
import qualified Data.Text as T

data UpdateResponse = UpdateResponse
  { ok :: Bool,
    result :: [Update]
  }
  deriving (Show, Eq)

instance FromJSON UpdateResponse where
  parseJSON (Object res) =
    UpdateResponse <$> res .: "ok"
      <*> res .: "result"
  parseJSON _ = mzero

data Update = Update
  { uUpdateId :: Integer,
    uMessage :: Message
  }
  deriving (Show, Eq)

instance FromJSON Update where
  parseJSON (Object update) =
    Update <$> update .: "update_id"
      <*> update .: "message"
  parseJSON _ = mzero

data Message = Message
  { mMessageId :: Integer,
    mFrom :: From,
    mChat :: Chat,
    mDate :: Integer,
    mText :: T.Text
  }
  deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object msg) =
    Message <$> msg .: "message_id"
      <*> msg .: "from"
      <*> msg .: "chat"
      <*> msg .: "date"
      <*> msg .: "text"
  parseJSON _ = mzero

data From = From
  { fId :: Integer,
    fIsBot :: Bool,
    fFirstName :: T.Text,
    fUsername :: T.Text,
    fLanguageCode :: T.Text
  }
  deriving (Show, Eq)

instance FromJSON From where
  parseJSON (Object from) =
    From <$> from .: "id"
      <*> from .: "is_bot"
      <*> from .: "first_name"
      <*> from .: "username"
      <*> from .: "language_code"
  parseJSON _ = mzero

data Chat = Chat
  { cId :: Integer,
    cFirstName :: T.Text,
    cUsername :: T.Text,
    cType :: T.Text
  }
  deriving (Show, Eq)

instance FromJSON Chat where
  parseJSON (Object chat) =
    Chat <$> chat .: "id"
      <*> chat .: "first_name"
      <*> chat .: "username"
      <*> chat .: "type"
  parseJSON _ = mzero

data SendMessageResponse = SendMessageResponse
  { smrOk :: Bool,
    smrErrorCode :: Maybe Int,
    smrDescription :: Maybe T.Text
  }
  deriving (Show, Eq)

instance FromJSON SendMessageResponse where
  parseJSON (Object req) =
    SendMessageResponse <$> req .: "ok"
      <*> (req .: "error_code" <|> pure Nothing)
      <*> (req .: "description" <|> pure Nothing)
  parseJSON _ = mzero
