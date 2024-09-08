-- |
-- Docs:
-- https://gavv.github.io/articles/pulseaudio-under-the-hood/#d-bus-api
--
-- https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/ConnectingToServer/
-- https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/
--
-- https://dbus.freedesktop.org/doc/dbus-tutorial.html
-- https://dbus.freedesktop.org/doc/dbus-specification.html
-- https://stackoverflow.com/questions/48648952/set-get-property-using-dbus-send
module XMWM.Sound.Pulse.DBus (
  PulseAudioT,
  runPulseAudioT,
  runPulseAudioT',
  fromPropList,
) where

import Relude

import Control.Exception.Safe (MonadCatch, bracket, catchAny)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.Map.Lazy qualified as Map
import Data.Text (dropEnd)
import DBus (Address)
import DBus.Client (Client, connect, disconnect)

-- | Carries the context for a D-Bus connection. Generally, you should be
-- using this on top of some sort of 'MonadIO'.
newtype PulseAudioT m a = PulseAudioT (ReaderT Client (ExceptT String m) a)
  deriving newtype (Monad, Applicative, Functor, MonadReader Client, MonadIO, MonadError String)

-- | Construct a new D-Bus connection and run the action over it. This will
-- automatically close the connection on completion or exception.
runPulseAudioT :: (MonadCatch m, MonadBaseControl IO m) => PulseAudioT m a -> Address -> m (Either String a)
runPulseAudioT action addr =
  liftBaseOp (bracket (connect addr) disconnect) (runPulseAudioT' action)

-- | Like 'runPulseAudioT', but you bring your own 'Client'. You're responsible
-- for making sure the client is closed correctly.
runPulseAudioT' :: (MonadCatch m) => PulseAudioT m a -> Client -> m (Either String a)
runPulseAudioT' (PulseAudioT r) client =
  runExceptT (runReaderT r client) `catchAny` (pure . Left . displayException)

-- | Get a value from a PulseAudio property list.
--
-- This function trims the trailing @NUL@ from values in the property list.
fromPropList :: (Monad m, Ord k) => k -> Map k ByteString -> m (Maybe Text)
fromPropList k m = pure $ dropEnd 1 . decodeUtf8 <$> Map.lookup k m
