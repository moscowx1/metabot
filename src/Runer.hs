module Runer (getRuner) where

import Config.Data (Config (..), Mode (Telegram, Terminal), unInfo)
import Control.Monad (forever)
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (toList)
import Handle (Handle, IMessage (id', message, setMessage), getRN)
import qualified Terminal.Runer as Telegram
import qualified Terminal.Runer as Terminal

send :: IMessage a => (a -> Handle ()) -> a -> Handle ()
send sender msg = do
  case message msg of
    "/help" -> do
      info <- asks (toList . unInfo . cInfo)
      sender (setMessage msg info)
    _ -> do
      rn <- getRN (id' msg)
      mapM_ (const $ sender msg) [1 .. rn]

runer :: IMessage m => Handle [m] -> (m -> Handle ()) -> Handle ()
runer getter sender = forever $ do
  resp <- getter
  mapM_ (send sender) resp

terminalRuner :: Handle ()
terminalRuner = runer Terminal.getter Terminal.sender

telegramRuner :: Handle ()
telegramRuner = runer Telegram.getter Telegram.sender

getRuner :: Mode -> Handle ()
getRuner Telegram = telegramRuner
getRuner Terminal = terminalRuner
