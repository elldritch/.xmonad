-- | Utilities for working with child processes.
module XMWM.Spawn (spawnDebug, spawnOutput) where

import Relude

import XMonad.Util.Run (runProcessWithInput)

import XMWM.Debug (debug)

-- | Spawn a command using 'spawnOutput', logging both the command and its
-- output using 'debug'.
spawnDebug :: (MonadIO m) => String -> String -> m String
spawnDebug msg cmd = do
  debug msg
  debug $ "running command: " ++ show cmd
  output <- liftIO $ spawnOutput cmd
  debug $ "command output: " ++ show output
  return output

-- | Synchronously spawn a command through @\/bin\/sh@, blocking until it has
-- terminated. Returns the command's STDOUT.
spawnOutput :: (MonadIO m) => String -> m String
spawnOutput s = runProcessWithInput "/bin/sh" ["-c", s] ""
