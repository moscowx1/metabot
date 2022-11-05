module TerminalRuner (sender, geter) where

import Control.Monad.Cont (lift)
import Data.Functor ((<&>))
import Handle (Handle, IMessage (id', message, setMessage))

lift' :: IO a -> Handle a
lift' = lift . lift . lift

sender :: TerminalMessage -> Handle ()
sender = lift' . putStrLn . unTerminalMessage

geter :: Handle [TerminalMessage]
geter = lift' $ getLine <&> pure . TerminalMessage

newtype TerminalMessage = TerminalMessage {unTerminalMessage :: String}
  deriving (Show)

instance IMessage TerminalMessage where
  id' = const 1
  message = unTerminalMessage
  setMessage _ = TerminalMessage