module XMWM.Prompt (dmenu) where

import Relude

import XMonad.Util.Dmenu qualified as Util

-- | Similar to `Util.dmenu`, but explicitly handling the case where users exit
-- without providing input.
dmenu :: (MonadIO m, ToString s) => [s] -> m (Maybe String)
dmenu options = do
  selected <- Util.dmenu $ toString <$> options
  pure $ if null selected then Nothing else Just selected
