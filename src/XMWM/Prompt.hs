module XMWM.Prompt (dmenu, dmenu') where

import Relude

import Control.Monad.Except (MonadError (throwError))
import Data.Map.Lazy qualified as Map
import XMonad.Util.Dmenu qualified as Util

-- | Similar to `Util.dmenu`, but explicitly handling the case where users exit
-- without providing input.
dmenu :: (MonadIO m, ToString s) => [s] -> m (Maybe String)
dmenu options = do
  selected <- Util.dmenu $ toString <$> options
  pure $ if null selected then Nothing else Just selected

-- | Similar to `dmenu`, except it takes a list of values and a rendering
-- function for those values. Returns an error message if the rendering function
-- creates the same string for any two values.
dmenu' :: forall m a. (MonadIO m, MonadError String m) => (a -> String) -> [a] -> m (Maybe a)
dmenu' render xs = do
  unless
    (length (ordNub rendered) == length rendered)
    $ throwError $ "dmenu': duplicate rendered elements: " <> show rendered
  selected <- dmenu rendered
  pure $ selected >>= (`Map.lookup` renderedToChoice)
  where
    rendered :: [String]
    rendered = render <$> xs

    renderedToChoice :: Map String a
    renderedToChoice = Map.fromList $ zip rendered xs
