module Terminal.Runer (getter, TerminalHandle, mapper) where

import Config.Data (Info, RepeatNum)
import Control.Monad.Cont (lift)
import Control.Monad.State (get)
import Data.Functor ((<&>))
import Handle (Handle)

lift' :: IO a -> TerminalHandle a
lift' = lift . lift . lift

sender :: String -> TerminalHandle ()
sender = lift' . putStrLn

getter :: TerminalHandle [TerminalMessage]
getter = lift' $ getLine <&> pure . TerminalMessage

mapper ::
  TerminalMessage ->
  TerminalHandle
    ( String,
      String -> TerminalHandle (),
      RepeatNum,
      TerminalHandle ()
    )
mapper (TerminalMessage msg) = do
  rn <- get
  pure (msg, sender, rn, pure ())

newtype TerminalMessage = TerminalMessage {unTerminalMessage :: String}
  deriving (Show)

type TerminalHandle = Handle Info RepeatNum
