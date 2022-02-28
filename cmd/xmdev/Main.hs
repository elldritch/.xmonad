-- | Useful for testing new utilities as a script instead when developing for
-- XMonad.
module Main (main) where

import Relude

import DBus (ObjectPath)
import Data.Map.Lazy qualified as Map
import Relude.Unsafe (fromJust)

import Sound.Pulse.DBus (PulseAudioT, runPulseAudioT)
import Sound.Pulse.DBus.Card (getDefaultSinkCardProfiles)
import Sound.Pulse.DBus.Server (getPulseAudioServerAddress)
import Sound.Pulse.DBus.Sink (Sink (..), getSinks, setDefaultSink)
import XMWM.Prompt (dmenu)

main :: IO ()
main = do
  -- testAction testPickSink
  testAction $ do
    profiles <- getDefaultSinkCardProfiles
    print profiles

testPickSink :: (MonadIO m) => PulseAudioT m ()
testPickSink = do
  sinks <- getSinks
  let nameToID = mapNameToID sinks
  selected <- toText . fromJust <$> dmenu (toString . sinkName <$> sinks)
  print selected
  let sinkID = fromJust $ Map.lookup selected nameToID
  print sinkID
  setDefaultSink sinkID
  where
    sinkName :: Sink -> Text
    sinkName Sink{name, description} = fromMaybe name description

    mapNameToID :: [Sink] -> Map Text ObjectPath
    mapNameToID sinks = Map.fromList $ (\s@Sink{sinkID} -> (sinkName s, sinkID)) <$> sinks

testAction :: Show a => PulseAudioT IO a -> IO ()
testAction action = do
  result <- runExceptT $ do
    addr <- ExceptT getPulseAudioServerAddress
    ExceptT $ runPulseAudioT action addr
  case result of
    Right r -> do
      putStrLn $ "Result:" <> show r
      putStrLn "OK"
    Left err -> putStrLn $ "ERROR: " <> err
