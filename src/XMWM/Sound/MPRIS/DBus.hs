module XMWM.Sound.MPRIS.DBus (MPRIST, runMPRIST) where

import Relude

import Control.Exception (bracket)
import Control.Exception.Safe (MonadCatch, catchAny)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import DBus.Client (Client, connectSession, disconnect)

-- | Carries the context for a D-Bus connection. Generally, you should be
-- using this on top of some sort of `MonadIO`.
newtype MPRIST m a = MPRIST (ReaderT Client (ExceptT String m) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Client, MonadError String)

-- | Construct a new D-Bus connection to the session bus and run the action over
-- it. This will automatically close the connection on completion or exception.
runMPRIST :: (MonadCatch m, MonadBaseControl IO m) => MPRIST m a -> m (Either String a)
runMPRIST action =
  liftBaseOp (bracket connectSession disconnect) (runMPRIST' action)

-- | Like `runMPRIST`, but you bring your own `Client`. You're responsible
-- for making sure the client is closed correctly.
runMPRIST' :: (MonadCatch m) => MPRIST m a -> Client -> m (Either String a)
runMPRIST' (MPRIST r) client =
  runExceptT (runReaderT r client) `catchAny` (pure . Left . displayException)
