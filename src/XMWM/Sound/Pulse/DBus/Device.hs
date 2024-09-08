module XMWM.Sound.Pulse.DBus.Device (
  Device (..),
  DeviceID,
  devicePrettyName,
  getSinks,
  setDefaultSink,
  getSources,
  setDefaultSource,
) where

import Relude

import DBus (IsVariant, MemberName, ObjectPath, Variant, toVariant)
import Data.Text (stripEnd, stripSuffix)

import XMWM.DBus (
  call,
  fromVariantMap,
  fromVariantMap',
  getAllProperties,
  getProperty,
  setProperty,
 )
import XMWM.Sound.Pulse.DBus (PulseAudioT, fromPropList)
import XMWM.Sound.Pulse.DBus.Interfaces (
  coreDefaultSinkProperty,
  coreDefaultSourceProperty,
  coreInterface,
  coreObject,
  corePlaybackStreamsProperty,
  coreRecordStreamsProperty,
  coreSinksProperty,
  coreSourcesProperty,
  deviceInterface,
  streamInterface,
  streamMoveMethod,
 )

data Device = Device
  { deviceID :: DeviceID
  , name :: Text
  , profileDescription :: Maybe Text
  , description :: Maybe Text
  , monitorOf :: Maybe ObjectPath
  }
  deriving (Show)

type DeviceID = ObjectPath

devicePrettyName :: Device -> Text
devicePrettyName Device{name, profileDescription, description} = fromMaybe name $ do
  desc <- description
  profDesc <- profileDescription
  stripEnd <$> stripSuffix profDesc desc

getCoreProperty :: (MonadIO m, IsVariant a) => MemberName -> PulseAudioT m a
getCoreProperty = getProperty coreObject coreInterface

getSinks :: (MonadIO m) => PulseAudioT m [Device]
getSinks = getCoreProperty coreSinksProperty >>= getDevices

getSources :: (MonadIO m) => PulseAudioT m [Device]
getSources = getCoreProperty coreSourcesProperty >>= getDevices

getDevices :: (MonadIO m) => [DeviceID] -> PulseAudioT m [Device]
getDevices deviceIDs = do
  forM deviceIDs $ \deviceID -> do
    deviceMap :: Map Text Variant <- getAllProperties deviceID deviceInterface
    name :: Text <- fromVariantMap "Name" deviceMap
    propsList :: Map Text ByteString <- fromVariantMap "PropertyList" deviceMap
    description <- fromPropList "device.description" propsList
    profileDescription <- fromPropList "device.profile.description" propsList
    monitorOf :: Maybe ObjectPath <- fromVariantMap' "MonitorOfSink" deviceMap
    pure Device{deviceID, name, description, profileDescription, monitorOf}

setDefaultSink :: (MonadIO m) => DeviceID -> PulseAudioT m ()
setDefaultSink sinkID = do
  getCoreProperty corePlaybackStreamsProperty >>= (`moveStreamsTo` sinkID)
  setProperty coreObject coreInterface coreDefaultSinkProperty sinkID

setDefaultSource :: (MonadIO m) => DeviceID -> PulseAudioT m ()
setDefaultSource sourceID = do
  getCoreProperty coreRecordStreamsProperty >>= (`moveStreamsTo` sourceID)
  setProperty coreObject coreInterface coreDefaultSourceProperty sourceID

type StreamID = ObjectPath

moveStreamsTo :: (MonadIO m) => [StreamID] -> DeviceID -> PulseAudioT m ()
moveStreamsTo streamIDs sinkID = forM_ streamIDs $ \streamID ->
  call streamID streamInterface streamMoveMethod [toVariant sinkID]
