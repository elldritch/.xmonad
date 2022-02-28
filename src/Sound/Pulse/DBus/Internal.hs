module Sound.Pulse.DBus.Internal (
  fromVariant',
  fromVariantMap,
  fromVariantMap',
  fromPropList,
  call,
  getProperty,
  getPropertyBus,
  getAllProperties,
  setProperty,
) where

import Relude

import Control.Monad.Except (liftEither, throwError)
import DBus (
  BusName,
  InterfaceName,
  IsValue,
  IsVariant (..),
  MemberName,
  MethodCall (..),
  MethodReturn (methodReturnBody),
  ObjectPath,
  Variant,
  methodCall,
 )
import DBus.Client (call_)
import Data.Map.Lazy qualified as Map
import Data.Text (dropEnd)

import Sound.Pulse.DBus (PulseAudioT)

-- | Unwrap a Variant value.
fromVariant' :: IsVariant a => Variant -> Either String a
fromVariant' = maybeToRight "fromVariant': bad cast" . fromVariant

-- | Get a value from a Variant dictionary.
fromVariantMap :: (Monad m, Ord k, Show k, IsVariant v) => k -> Map k Variant -> PulseAudioT m v
fromVariantMap k m =
  liftEither $
    fromVariant'
      =<< maybeToRight
        ("fromVariantMap: no key " <> show k)
        (Map.lookup k m)

-- | Get a possibly missing value from a Variant dictionary.
fromVariantMap' :: (Monad m, Ord k, IsVariant v) => k -> Map k Variant -> PulseAudioT m (Maybe v)
fromVariantMap' k m = do
  case Map.lookup k m of
    Just v -> Just <$> liftEither (fromVariant' v)
    Nothing -> pure Nothing

-- | Get a value from a PulseAudio property list.
--
-- This function trims the trailing @NUL@ from values in the property list.
fromPropList :: (Monad m, Ord k) => k -> Map k ByteString -> PulseAudioT m (Maybe Text)
fromPropList k m = pure $ dropEnd 1 . decodeUtf8 <$> Map.lookup k m

call' :: (MonadIO m) => MethodCall -> PulseAudioT m [Variant]
call' msg = do
  client <- ask
  reply <- liftIO $ call_ client $ msg{methodCallReplyExpected = True}
  pure $ methodReturnBody reply

-- | Call a method.
call :: (MonadIO m) => ObjectPath -> InterfaceName -> MemberName -> [Variant] -> PulseAudioT m [Variant]
call object interface method args =
  call' (methodCall object interface method){methodCallBody = args}

-- | Call a method that does not return a value.
call0 :: (MonadIO m) => MethodCall -> PulseAudioT m ()
call0 = void . call'

-- | Call a method that returns a single value.
call1 :: (MonadIO m, IsVariant a) => MethodCall -> PulseAudioT m a
call1 msg = do
  reply <- call' msg
  case decode reply of
    Right result -> pure result
    Left err -> throwError $ "call1: " <> err
  where
    decode :: IsVariant a => [Variant] -> Either String a
    decode reply = fromVariant' =<< hasOneElem reply

    hasOneElem :: [a] -> Either String a
    hasOneElem = \case
      [x] -> Right x
      _ -> Left "hasOneElem: method return did not have one element"

propertiesInterface :: InterfaceName
propertiesInterface = "org.freedesktop.DBus.Properties"

getMethod :: MemberName
getMethod = "Get"

getAllMethod :: MemberName
getAllMethod = "GetAll"

setMethod :: MemberName
setMethod = "Set"

-- | Call the standard \"Get\" method.
--
-- Results from \"Get\" are always wrapped in a `Variant`. See also:
--
-- - https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties
getProperty :: (MonadIO m, IsVariant a) => ObjectPath -> InterfaceName -> MemberName -> PulseAudioT m a
getProperty object interface property = do
  v :: Variant <- call1 $ getPropertyCall object interface property
  either (\err -> throwError $ "getProperty: " <> err) pure $ fromVariant' v

getPropertyBus :: (MonadIO m, IsVariant a) => BusName -> ObjectPath -> InterfaceName -> MemberName -> PulseAudioT m a
getPropertyBus bus object interface property = do
  v :: Variant <-
    call1 (getPropertyCall object interface property){methodCallDestination = Just bus}
  either (\err -> throwError $ "getPropertyBus: " <> err) pure $ fromVariant' v

getPropertyCall :: ObjectPath -> InterfaceName -> MemberName -> MethodCall
getPropertyCall object interface property =
  (methodCall object propertiesInterface getMethod){methodCallBody = [toVariant interface, toVariant property]}

-- | Call the standard \"GetAll\" method.
--
-- Results from \"GetAll\" are always an @ARRAY of DICT_ENTRY<STRING,VARIANT>@.
-- See also:
--
-- - https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties
getAllProperties :: (MonadIO m) => ObjectPath -> InterfaceName -> PulseAudioT m (Map Text Variant)
getAllProperties object interface = do
  call1 (methodCall object propertiesInterface getAllMethod){methodCallBody = [toVariant interface]}

-- | Call the standard \"Set\" method.
setProperty :: (MonadIO m, IsValue a) => ObjectPath -> InterfaceName -> MemberName -> a -> PulseAudioT m ()
setProperty object interface property value = do
  call0
    (methodCall object propertiesInterface setMethod)
      { methodCallBody = [toVariant interface, toVariant property, toVariant $ toVariant value]
      }
