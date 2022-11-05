module Core (getRuner) where

import Config.Core (cInfo)
import Config.Data (Mode (Telegram, Terminal), unInfo)
import Control.Monad (forever)
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (toList)
import Handle (Handle, IMessage (id', message, setMessage), getRN)
import qualified TerminalRuner as T
import qualified WebRuner as W

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
terminalRuner = runer T.geter T.sender

webRuner :: Handle ()
webRuner = runer W.getter W.sender

getRuner :: Mode -> Handle ()
getRuner Telegram = webRuner
getRuner Terminal = terminalRuner
