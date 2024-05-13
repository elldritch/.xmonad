-- | Utilities for working with XMobar.
module XMWM.XMobar (logWindowsToXMobar) where

import Relude

import XMonad.Core (X, withWindowSet)
import XMonad.Hooks.StatusBar.PP (PP (..), dynamicLogWithPP, shorten, xmobarColor, xmobarPP)
import XMonad.StackSet (index, peek)
import XMonad.Util.Loggers (Logger)
import XMonad.Util.NamedWindows (getName, unName)
import XMonad.Util.Run (hPutStrLn)

-- | A log hook. Given the handle for XMobar, this pretty-prints the output that
-- XMobar expects for workspace and window display.
logWindowsToXMobar :: Handle -> X ()
logWindowsToXMobar xmobarHandle =
  dynamicLogWithPP
    xmobarPP
      { ppOutput = hPutStrLn xmobarHandle
      , ppTitle = xmobarColor "lime" ""
      , ppExtras = [logTitles (xmobarColor "lime" "")]
      , ppOrder = \case
          (workspaces : layout : _ : extras) -> [workspaces, layout] ++ extras
          _ -> error "impossible: xmobar pretty-print args malformed"
      }
  where
    logTitles :: (String -> String) -> Logger
    logTitles ppFocus =
      withWindowSet $ fmap (Just . intercalate " | ") . windowTitles
      where
        windowName = shorten 30 . show
        isFocused windowSet target = (== Just (unName target)) $ peek windowSet
        highlightFocused windowSet target =
          (if isFocused windowSet target then ppFocus else id) $ windowName target
        namedWindows = fmap getName . index
        windowTitles windowSet =
          mapM (fmap $ highlightFocused windowSet) $ namedWindows windowSet
