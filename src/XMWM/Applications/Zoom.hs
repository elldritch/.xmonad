module XMWM.Applications.Zoom (zoomManageHook, zoomEventHook) where

import Relude

import XMonad.Config.Prime (Event)
import XMonad.Core (ManageHook, X)
import XMonad.ManageHook (className, composeAll, doF, doFloat, title, (-->), (<&&>), (<+>), (=?))
import XMonad.StackSet qualified as W
import XMonad.Hooks.DynamicProperty (dynamicTitle)

-- Adapted from https://www.peterstuart.org/posts/2021-09-06-xmonad-zoom/

zoomManageHook :: ManageHook
zoomManageHook =
  composeAll
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat
    , (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account" -- main window
      , "Zoom - Licensed Account" -- main window
      , "Zoom" -- meeting window on creation
      , "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat = (`notElem` tileTitles)
    shouldSink = (`elem` tileTitles)
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

zoomEventHook :: Event -> X All
zoomEventHook = dynamicTitle zoomManageHook
