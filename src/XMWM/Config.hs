{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | The XMonad configuration.
module XMWM.Config (xConf) where

import Relude

import Data.Default (def)
import XMonad.Core (XConfig (..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.NoBorders (smartBorders)

import XMWM.Keybindings (superMask)
import XMWM.Workspaces (defaultWorkspaces)
import XMWM.XMobar (logWindowsToXMobar)

-- | The customized XMonad configuration to use.
xConf :: Handle -> XConfig _
xConf outputHandle =
  def
    { borderWidth = 2
    , modMask = superMask
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , terminal = "terminator"
    , workspaces = snd <$> defaultWorkspaces
    , -- Workaround for Java Swing applications: https://stackoverflow.com/questions/30742662/java-swing-gui-not-displaying-in-xmonad
      startupHook = setWMName "LG3D"
    , manageHook = mconcat [manageDocks, placeHook simpleSmart, manageHook def]
    , layoutHook = avoidStruts $ smartBorders $ layoutHook def
    , logHook = logWindowsToXMobar outputHandle
    }
