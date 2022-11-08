{-# LANGUAGE FlexibleContexts #-}

module Telegram.State (TelegramSt (..), telegramState) where

import Config.Data (RepeatNum)
import Telegram.Data (ChatId)

type Offset = Int

telegramState :: TelegramSt
telegramState = TelegramSt 0 []

data TelegramSt = TelegramSt
  { tOffset :: Offset,
    tIdToRN :: [(ChatId, RepeatNum)]
  }