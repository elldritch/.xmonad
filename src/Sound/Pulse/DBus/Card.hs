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

type CardProfileID = ObjectPath

getDefaultSinkCard :: (MonadIO m) => PulseAudioT m CardProfileID
getDefaultSinkCard = do
  defaultSinkPath :: ObjectPath <- getProperty coreObject coreInterface coreDefaultSinkProperty
  getProperty defaultSinkPath deviceInterface deviceCardProperty

getDefaultSinkCardProfiles :: (MonadIO m) => PulseAudioT m [CardProfile]
getDefaultSinkCardProfiles = do
  defaultSinkCardPath <- getDefaultSinkCard
  profilePaths :: [ObjectPath] <- getProperty defaultSinkCardPath cardInterface cardProfilesProperty
  fmap (filter available) $
    forM profilePaths $ \profilePath -> do
      cardProfileMap :: Map Text Variant <- getAllProperties profilePath cardProfileInterface
      name :: Text <- fromVariantMap "Name" cardProfileMap
      description :: Text <- fromVariantMap "Description" cardProfileMap
      available :: Bool <- fromVariantMap "Available" cardProfileMap
      pure CardProfile{profileID = profilePath, name, description, available}

setDefaultSinkCardProfile :: (MonadIO m) => CardProfileID -> PulseAudioT m ()
setDefaultSinkCardProfile profileID = do
  defaultSinkCardPath <- getDefaultSinkCard
  setProperty defaultSinkCardPath cardInterface cardActiveProfileProperty profileID

getDefaultSourceCardProfiles = undefined

setDefaultSourceCardProfile = undefined
