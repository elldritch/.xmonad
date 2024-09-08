module XMWM.Sound.MPRIS.DBus.Interfaces (
  mprisObjectPath,
  mprisInterface,
  identityProperty,
  playerInterface,
  playPauseMethod,
  playMethod,
  pauseMethod,
  nextMethod,
  previousMethod,
) where

import DBus (InterfaceName, MemberName, ObjectPath)

mprisObjectPath :: ObjectPath
mprisObjectPath = "/org/mpris/MediaPlayer2"

mprisInterface :: InterfaceName
mprisInterface = "org.mpris.MediaPlayer2"

identityProperty :: MemberName
identityProperty = "Identity"

playerInterface :: InterfaceName
playerInterface = "org.mpris.MediaPlayer2.Player"

playPauseMethod :: MemberName
playPauseMethod = "PlayPause"

playMethod :: MemberName
playMethod = "Play"

pauseMethod :: MemberName
pauseMethod = "Pause"

nextMethod :: MemberName
nextMethod = "Next"

previousMethod :: MemberName
previousMethod = "Previous"
