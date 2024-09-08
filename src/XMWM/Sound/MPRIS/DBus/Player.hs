module XMWM.Sound.MPRIS.DBus.Player (
  Player (..),
  listPlayers,
  playPause,
  play,
  pause,
  next,
  previous,
) where

import Relude

import DBus (BusName, MemberName, methodCall, methodCallDestination)

import XMWM.DBus (call0, call1, getPropertyBus)
import XMWM.DBus.Interfaces (busInterface, busObjectPath, busServiceName, listNamesMethod)
import XMWM.Sound.MPRIS.DBus (MPRIST)
import XMWM.Sound.MPRIS.DBus.Interfaces (identityProperty, mprisInterface, mprisObjectPath, nextMethod, pauseMethod, playMethod, playPauseMethod, playerInterface, previousMethod)

data Player = Player
  { busName :: BusName
  , identity :: String
  }

-- | List the services that implement the MPRIS interface.
listPlayers :: (MonadIO m) => MPRIST m [Player]
listPlayers = do
  busNames :: [String] <-
    call1
      (methodCall busObjectPath busInterface listNamesMethod)
        { methodCallDestination = Just busServiceName
        }
  let mprisServices = fromString <$> filter ("org.mpris.MediaPlayer2." `isPrefixOf`) busNames
  traverse
    ( \service -> do
        mrpisIdentity :: String <- getPropertyBus service mprisObjectPath mprisInterface identityProperty
        pure $ Player service mrpisIdentity
    )
    mprisServices

playPause :: (MonadIO m) => Player -> MPRIST m ()
playPause = playerCall playPauseMethod

play :: (MonadIO m) => Player -> MPRIST m ()
play = playerCall playMethod

pause :: (MonadIO m) => Player -> MPRIST m ()
pause = playerCall pauseMethod

next :: (MonadIO m) => Player -> MPRIST m ()
next = playerCall nextMethod

previous :: (MonadIO m) => Player -> MPRIST m ()
previous = playerCall previousMethod

playerCall :: (MonadIO m) => MemberName -> Player -> MPRIST m ()
playerCall method player =
  call0
    (methodCall mprisObjectPath playerInterface method)
      { methodCallDestination = Just player.busName
      }
