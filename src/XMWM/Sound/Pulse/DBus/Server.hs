-- | Utilities for working with PulseAudio server lookup.
module XMWM.Sound.Pulse.DBus.Server (
  getPulseAudioServerAddress,
  runPulseAudioTSession,
) where

import Relude

import Control.Exception (bracket)
import Control.Monad.Except (throwError)
import DBus (
  Address,
  BusName,
  InterfaceName,
  MemberName,
  ObjectPath,
  parseAddress,
 )
import DBus.Client (connectSession, disconnect)

import XMWM.Sound.Pulse.DBus (PulseAudioT, runPulseAudioT, runPulseAudioT')
import XMWM.DBus (getPropertyBus)

serverLookupDest :: BusName
serverLookupDest = "org.PulseAudio1"

serverLookupObject :: ObjectPath
serverLookupObject = "/org/pulseaudio/server_lookup1"

serverLookupInterface :: InterfaceName
serverLookupInterface = "org.PulseAudio.ServerLookup1"

addressProperty :: MemberName
addressProperty = "Address"

-- | Connects to the D-Bus session bus and look up the address of this session's
-- PulseAudio server's D-Bus API.
--
-- PulseAudio uses peer-to-peer D-Bus mode. Most control operations are only
-- exposed on the PulseAudio server's own D-Bus API. However, PulseAudio exposes
-- a single \"server lookup\" API for clients to look up the PulseAudio server's
-- address. Afterwards, clients should connect directly to PulseAudio's D-Bus
-- server to call control methods.
--
-- For more details, see:
--
-- - https://gavv.github.io/articles/pulseaudio-under-the-hood/#d-bus-api
-- - https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/ConnectingToServer/
getPulseAudioServerAddress :: IO (Either String Address)
getPulseAudioServerAddress = do
  bracket connectSession disconnect $
    runPulseAudioT' $ do
      reply <- getPropertyBus serverLookupDest serverLookupObject serverLookupInterface addressProperty
      maybe (throwError "getPulseAudioServerAddress: did not return a valid address") pure $ parseAddress reply

-- | Like 'runPulseAudioT', but instead of specifying an address, automatically
-- query the session D-Bus instance for PulseAudio's D-Bus server address.
runPulseAudioTSession :: PulseAudioT IO a -> IO (Either String a)
runPulseAudioTSession action = runExceptT $ do
  addr <- ExceptT $ liftIO getPulseAudioServerAddress
  ExceptT $ runPulseAudioT action addr
