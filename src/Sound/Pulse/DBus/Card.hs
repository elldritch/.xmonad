module Sound.Pulse.DBus.Card (
  CardProfile (..),
  getDefaultSinkCardProfiles,
  setDefaultSinkCardProfile,
  getDefaultSourceCardProfiles,
  setDefaultSourceCardProfile,
) where

import Relude

import DBus (ObjectPath, Variant)

import Sound.Pulse.DBus (PulseAudioT)
import Sound.Pulse.DBus.Interfaces (
  cardActiveProfileProperty,
  cardInterface,
  cardProfileInterface,
  cardProfilesProperty,
  coreDefaultSinkProperty,
  coreDefaultSourceProperty,
  coreInterface,
  coreObject,
  deviceCardProperty,
  deviceInterface,
 )
import Sound.Pulse.DBus.Internal (fromVariantMap, getAllProperties, getProperty, setProperty)

data CardProfile = CardProfile
  { profileID :: CardProfileID
  , name :: Text
  , description :: Text
  , available :: Bool
  }
  deriving (Show)

type CardID = ObjectPath

type CardProfileID = ObjectPath

getDefaultSinkCard :: (MonadIO m) => PulseAudioT m CardID
getDefaultSinkCard = do
  defaultSinkPath :: ObjectPath <- getProperty coreObject coreInterface coreDefaultSinkProperty
  getProperty defaultSinkPath deviceInterface deviceCardProperty

getDefaultSourceCard :: (MonadIO m) => PulseAudioT m CardID
getDefaultSourceCard = do
  defaultSourcePath :: ObjectPath <- getProperty coreObject coreInterface coreDefaultSourceProperty
  getProperty defaultSourcePath deviceInterface deviceCardProperty

getCardProfiles :: (MonadIO m) => CardID -> PulseAudioT m [CardProfile]
getCardProfiles cardID = do
  profilePaths :: [ObjectPath] <- getProperty cardID cardInterface cardProfilesProperty
  fmap (filter available) $
    forM profilePaths $ \profilePath -> do
      cardProfileMap :: Map Text Variant <- getAllProperties profilePath cardProfileInterface
      name :: Text <- fromVariantMap "Name" cardProfileMap
      description :: Text <- fromVariantMap "Description" cardProfileMap
      available :: Bool <- fromVariantMap "Available" cardProfileMap
      pure CardProfile{profileID = profilePath, name, description, available}

getDefaultSinkCardProfiles :: (MonadIO m) => PulseAudioT m [CardProfile]
getDefaultSinkCardProfiles = getDefaultSinkCard >>= getCardProfiles

getDefaultSourceCardProfiles :: (MonadIO m) => PulseAudioT m [CardProfile]
getDefaultSourceCardProfiles = getDefaultSourceCard >>= getCardProfiles

setCardProfile :: (MonadIO m) => CardID -> CardProfileID -> PulseAudioT m ()
setCardProfile cardID = setProperty cardID cardInterface cardActiveProfileProperty

setDefaultSinkCardProfile :: (MonadIO m) => CardProfileID -> PulseAudioT m ()
setDefaultSinkCardProfile profileID = getDefaultSinkCard >>= (`setCardProfile` profileID)

setDefaultSourceCardProfile :: (MonadIO m) => CardProfileID -> PulseAudioT m ()
setDefaultSourceCardProfile profileID = getDefaultSourceCard >>= (`setCardProfile` profileID)
