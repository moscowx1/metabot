module Handle (Handle) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Servant.Client (ClientError)

type Handle env st = ReaderT env (StateT st (ExceptT ClientError IO))