module Sound.Pulse.DBus.Interfaces (
  -- * Core
  coreObject,
  coreInterface,
  coreSinksProperty,
  coreSourcesProperty,
  coreDefaultSinkProperty,
  coreDefaultSourceProperty,
  corePlaybackStreamsProperty,
  coreRecordStreamsProperty,

  -- * Card
  cardInterface,
  cardProfilesProperty,
  cardActiveProfileProperty,

  -- * Card Profile
  cardProfileInterface,
  cardProfileNameProperty,
  cardProfileDescriptionProperty,

  -- * Device (source, sink)
  deviceInterface,
  deviceCardProperty,

  -- * Stream (source-output, sink-input)
  streamInterface,
  streamMoveMethod,
) where

import DBus (InterfaceName, MemberName, ObjectPath)

-- Core

coreObject :: ObjectPath
coreObject = "/org/pulseaudio/core1"

coreInterface :: InterfaceName
coreInterface = "org.PulseAudio.Core1"

coreSinksProperty :: MemberName
coreSinksProperty = "Sinks"

coreSourcesProperty :: MemberName
coreSourcesProperty = "Sources"

coreDefaultSinkProperty :: MemberName
coreDefaultSinkProperty = "FallbackSink"

coreDefaultSourceProperty :: MemberName
coreDefaultSourceProperty = "FallbackSource"

corePlaybackStreamsProperty :: MemberName
corePlaybackStreamsProperty = "PlaybackStreams"

coreRecordStreamsProperty :: MemberName
coreRecordStreamsProperty = "RecordStreams"

-- Card

cardInterface :: InterfaceName
cardInterface = "org.PulseAudio.Core1.Card"

cardProfilesProperty :: MemberName
cardProfilesProperty = "Profiles"

cardActiveProfileProperty :: MemberName
cardActiveProfileProperty = "ActiveProfile"

-- Card Profiles

cardProfileInterface :: InterfaceName
cardProfileInterface = "org.PulseAudio.Core1.CardProfile"

cardProfileNameProperty :: MemberName
cardProfileNameProperty = "Name"

cardProfileDescriptionProperty :: MemberName
cardProfileDescriptionProperty = "Description"

-- Device

deviceInterface :: InterfaceName
deviceInterface = "org.PulseAudio.Core1.Device"

deviceCardProperty :: MemberName
deviceCardProperty = "Card"

-- Stream

streamInterface :: InterfaceName
streamInterface = "org.PulseAudio.Core1.Stream"

streamMoveMethod :: MemberName
streamMoveMethod = "Move"
