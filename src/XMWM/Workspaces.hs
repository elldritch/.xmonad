-- | Utilities for workspaces and user input for workspaces.
module XMWM.Workspaces (defaultWorkspaces, workspaceFromDmenu) where

import Relude

import Data.Set qualified as Set

import Graphics.X11.Types (KeySym)
import Graphics.X11.Xlib (
  xK_0,
  xK_1,
  xK_9,
  xK_BackSpace,
  xK_equal,
  xK_grave,
  xK_minus,
 )
import XMonad.Core (WorkspaceId, X, withWindowSet)
import XMonad.StackSet (Workspace (tag), workspaces)
import XMonad.Util.Dmenu (dmenu)

-- | A static list of default workspaces to choose from. This is every key
-- across the top row, from tilde to backspace.
defaultWorkspaces :: [(KeySym, WorkspaceId)]
defaultWorkspaces =
  zip
    ([xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace])
    ((["~"] ++ fmap show [1 .. 9 :: Int]) ++ ["0", "-", "=", "B"])

-- | Prompt the user with @dmenu@ to choose from a list of existing workspaces.
-- Users can also enter a new workspace ID (which is just a string). Returns
-- Nothing if the user exited the prompt without selection.
workspaceFromDmenu :: X (Maybe WorkspaceId)
workspaceFromDmenu = do
  currentWorkspaces <- withWindowSet (pure . workspaces)
  let currentWorkspaceNames = tag <$> currentWorkspaces
  let customWorkspaceNames = sort $ filter (`Set.notMember` defaultWorkspacesSet) currentWorkspaceNames
  selected <- dmenu $ (snd <$> defaultWorkspaces) ++ customWorkspaceNames
  pure $ if null selected then Nothing else Just selected
  where
    defaultWorkspacesSet = Set.fromList $ snd <$> defaultWorkspaces
