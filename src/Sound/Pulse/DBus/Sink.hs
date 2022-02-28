module Sound.Pulse.DBus.Sink (
  Sink (..),
  SinkID,
  getSinks,
  setDefaultSink,
) where

import Relude

import DBus (ObjectPath, Variant, toVariant)

import Sound.Pulse.DBus (PulseAudioT)
import Sound.Pulse.DBus.Interfaces (
  coreDefaultSinkProperty,
  coreInterface,
  coreObject,
  corePlaybackStreamsProperty,
  coreSinksProperty,
  deviceInterface,
  streamInterface,
  streamMoveMethod,
 )
import Sound.Pulse.DBus.Internal (
  call,
  fromPropList,
  fromVariantMap,
  getAllProperties,
  getProperty,
  setProperty,
 )

data Sink = Sink
  { sinkID :: SinkID
  , name :: Text
  , description :: Maybe Text
  }
  deriving (Show)

type SinkID = ObjectPath

getSinks :: (MonadIO m) => PulseAudioT m [Sink]
getSinks = do
  sinkPaths :: [ObjectPath] <- getProperty coreObject coreInterface coreSinksProperty
  forM sinkPaths $ \sinkID -> do
    sinkMap :: Map Text Variant <- getAllProperties sinkID deviceInterface
    name :: Text <- fromVariantMap "Name" sinkMap
    propsList :: Map Text ByteString <- fromVariantMap "PropertyList" sinkMap
    description <- fromPropList "device.description" propsList
    pure Sink{sinkID, name, description}

setDefaultSink :: (MonadIO m) => SinkID -> PulseAudioT m ()
setDefaultSink sinkID = do
  movePlaybackStreamsTo sinkID
  setProperty coreObject coreInterface coreDefaultSinkProperty sinkID

movePlaybackStreamsTo :: (MonadIO m) => SinkID -> PulseAudioT m ()
movePlaybackStreamsTo sinkID = do
  pbStreamPaths :: [ObjectPath] <- getProperty coreObject coreInterface corePlaybackStreamsProperty
  forM_ pbStreamPaths $ \pbStreamPath -> do
    call pbStreamPath streamInterface streamMoveMethod [toVariant sinkID]