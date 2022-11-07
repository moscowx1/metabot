module Handle
  ( Handle,
  )
where

import Config.Data (Config (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Servant.Client (ClientError)

type Handle st = ReaderT Config (StateT st (ExceptT ClientError IO))