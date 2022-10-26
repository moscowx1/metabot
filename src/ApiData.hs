{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiData where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import GHC.Generics (Generic)

data Updates = Updates
  { ok :: Bool,
    result :: [Update]
  }
  deriving (Generic, FromJSON, Show)

data Update = Update
  { updateId :: Int,
    updateMessage :: Message
  }
  deriving (Show)

instance FromJSON Update where
  parseJSON (Object update) =
    Update
      <$> update
      .: "update_id"
      <*> update
      .: "message"
  parseJSON _ = mzero

data Message = Message
  { messageId :: Int,
    messageFrom :: From,
    messageChat :: Chat,
    messageDate :: Int,
    messageText :: String
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
  { fromId :: Int,
    fromIsBot :: Bool,
    fromFirstName :: String,
    fromLastName :: String,
    fromLanguageCode :: String
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
  { chatId :: Int,
    chatFirstName :: String,
    chatUserName :: String,
    chatType :: String
  }
  deriving (Show, Eq)

instance FromJSON Chat where
  parseJSON (Object chat) =
    Chat <$> chat .: "id"
      <*> chat .: "first_name"
      <*> chat .: "username"
      <*> chat .: "type"
  parseJSON _ = mzero

data MessageResponse = MessageResponse
  { mrOk :: Bool,
    mrErrorCode :: Maybe Int,
    mrDecription :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON MessageResponse where
  parseJSON (Object req) =
    MessageResponse <$> req .: "ok"
      <*> (req .: "error_code" <|> pure Nothing)
      <*> (req .: "description" <|> pure Nothing)
  parseJSON _ = mzero
