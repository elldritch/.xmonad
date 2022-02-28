-- |
-- Docs:
--
-- https://dbus.freedesktop.org/doc/dbus-tutorial.html
-- https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/ConnectingToServer/
-- https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/#devicessinksandsources
-- https://stackoverflow.com/questions/48648952/set-get-property-using-dbus-send
-- https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties
-- https://gavv.github.io/articles/pulseaudio-under-the-hood/#d-bus-api
module Sound.Pulse.DBus (
  PulseAudioT,
  runPulseAudioT,
  runPulseAudioT',
) where

import Relude

import Control.Exception.Safe (MonadCatch, bracket, catchAny)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import DBus (Address)
import DBus.Client (Client, connect, disconnect)

-- | Carries the context for a D-Bus connection. Generally, you should be
-- using this on top of some sort of `MonadIO`.
newtype PulseAudioT m a = PulseAudioT (ReaderT Client (ExceptT String m) a)
  deriving newtype (Monad, Applicative, Functor, MonadReader Client, MonadIO, MonadError String)

-- | Construct a new D-Bus connection and run the action over it. This will
-- automatically close the connection on completion or exception.
runPulseAudioT :: (MonadCatch m, MonadBaseControl IO m) => PulseAudioT m a -> Address -> m (Either String a)
runPulseAudioT action addr =
  liftBaseOp (bracket (connect addr) disconnect) (runPulseAudioT' action)

-- | Like `runPulseAudioT`, but you bring your own `Client`. You're responsible
-- for making sure the client is closed correctly.
runPulseAudioT' :: (MonadCatch m) => PulseAudioT m a -> Client -> m (Either String a)
runPulseAudioT' (PulseAudioT r) client =
  runExceptT (runReaderT r client) `catchAny` (pure . Left . displayException)
