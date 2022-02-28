-- | Utilities for debugging.
module XMWM.Debug (debug) where

import Relude

import XMonad.Util.Run (hPutStrLn)

-- | Log a debug message to STDERR. @lightdm@ will output the STDERR of window
-- managers to its own STDERR, which gets piped by X11 to @~/.xsession-errors@.
debug :: (MonadIO m) => String -> m ()
debug = liftIO . hPutStrLn stderr
