module XMWM.DBus (
  fromVariant',
  fromVariantMap,
  fromVariantMap',
  call,
  call0,
  call1,
  getProperty,
  getPropertyBus,
  getAllProperties,
  setProperty,
) where

import Relude

import Control.Monad.Except (MonadError, liftEither, throwError)
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
import DBus.Client (Client, call_)
import Data.Map.Lazy qualified as Map

import XMWM.DBus.Interfaces (getAllMethod, getMethod, propertiesInterface, setMethod)

-- | Unwrap a 'Variant' value.
fromVariant' :: (IsVariant a) => Variant -> Either String a
fromVariant' = maybeToRight "fromVariant': bad cast" . fromVariant

-- | Get a value from a 'Variant' dictionary.
fromVariantMap :: (MonadError String m, Ord k, Show k, IsVariant v) => k -> Map k Variant -> m v
fromVariantMap k m =
  liftEither $
    fromVariant'
      =<< maybeToRight
        ("fromVariantMap: no key " <> show k)
        (Map.lookup k m)

-- | Get a possibly missing value from a 'Variant' dictionary.
fromVariantMap' :: (MonadError String m, Ord k, IsVariant v) => k -> Map k Variant -> m (Maybe v)
fromVariantMap' k m = do
  case Map.lookup k m of
    Just v -> Just <$> liftEither (fromVariant' v)
    Nothing -> pure Nothing

call' :: (MonadIO m, MonadReader Client m) => MethodCall -> m [Variant]
call' msg = do
  client <- ask
  reply <- liftIO $ call_ client $ msg{methodCallReplyExpected = True}
  pure $ methodReturnBody reply

-- | Call a method.
call :: (MonadIO m, MonadReader Client m) => ObjectPath -> InterfaceName -> MemberName -> [Variant] -> m [Variant]
call object interface method args =
  call' (methodCall object interface method){methodCallBody = args}

-- | Call a method that does not return a value.
call0 :: (MonadIO m, MonadReader Client m) => MethodCall -> m ()
call0 = void . call'

-- | Call a method that returns a single value.
call1 :: (MonadIO m, MonadReader Client m, MonadError String m, IsVariant a) => MethodCall -> m a
call1 msg = do
  reply <- call' msg
  case decode reply of
    Right result -> pure result
    Left err -> throwError $ "call1: " <> err
  where
    decode :: (IsVariant a) => [Variant] -> Either String a
    decode reply = fromVariant' =<< hasOneElem reply

    hasOneElem :: [a] -> Either String a
    hasOneElem = \case
      [x] -> Right x
      _ -> Left "hasOneElem: method return did not have one element"

-- | Call the standard \"Get\" method.
--
-- Results from \"Get\" are always wrapped in a 'Variant'. See also:
--
-- - https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties
getProperty :: (MonadIO m, MonadReader Client m, MonadError String m, IsVariant a) => ObjectPath -> InterfaceName -> MemberName -> m a
getProperty object interface property = do
  v :: Variant <- call1 $ getPropertyCall object interface property
  either (\err -> throwError $ "getProperty: " <> err) pure $ fromVariant' v

-- | Like 'getProperty', but for a specific bus.
getPropertyBus :: (MonadIO m, MonadReader Client m, MonadError String m, IsVariant a) => BusName -> ObjectPath -> InterfaceName -> MemberName -> m a
getPropertyBus bus object interface property = do
  v :: Variant <-
    call1 (getPropertyCall object interface property){methodCallDestination = Just bus}
  either (\err -> throwError $ "getPropertyBus: " <> err) pure $ fromVariant' v

getPropertyCall :: ObjectPath -> InterfaceName -> MemberName -> MethodCall
getPropertyCall object interface property =
  (methodCall object propertiesInterface getMethod){methodCallBody = [toVariant interface, toVariant property]}

-- | Call the standard \"GetAll\" method.
--
-- Results from \"GetAll\" are always an @ARRAY of DICT_ENTRY\<STRING,VARIANT\>@.
-- See also:
--
-- - https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties
getAllProperties :: (MonadIO m, MonadReader Client m, MonadError String m) => ObjectPath -> InterfaceName -> m (Map Text Variant)
getAllProperties object interface = do
  call1 (methodCall object propertiesInterface getAllMethod){methodCallBody = [toVariant interface]}

-- | Call the standard \"Set\" method.
setProperty :: (MonadIO m, MonadReader Client m, IsValue a) => ObjectPath -> InterfaceName -> MemberName -> a -> m ()
setProperty object interface property value = do
  call0
    (methodCall object propertiesInterface setMethod)
      { methodCallBody = [toVariant interface, toVariant property, toVariant $ toVariant value]
      }
