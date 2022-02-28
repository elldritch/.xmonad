module Sound.Pulse.DBus.Source (
  Source (..),
  SourceID,
  getSources,
  setDefaultSource,
) where

import Relude

import DBus (ObjectPath)

import Sound.Pulse.DBus (PulseAudioT)

data Source = Source

type SourceID = ObjectPath

getSources :: (MonadIO m) => PulseAudioT m [Source]
getSources = undefined

setDefaultSource :: (MonadIO m) => SourceID -> PulseAudioT m ()
setDefaultSource = undefined

moveRecordingStreamsTo = undefined
