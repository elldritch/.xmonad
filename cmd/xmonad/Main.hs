module Main (main) where

import Relude

import XMonad.Hooks.ManageDocks (docks)
import XMonad.Main (xmonad)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

import XMWM.Config (xConf)
import XMWM.Keybindings (keybindings)

main :: IO ()
main = do
  xmobarHandle <- spawnPipe "xmobar"
  xmonad $ docks $ xConf xmobarHandle `additionalKeys` keybindings

-- Scale the UI for high-DPI displays.
--
-- TODO: read xdpyinfo? Or maybe not?
--
-- https://unix.stackexchange.com/questions/75344/how-does-x-server-calculate-dpi
--
-- Ugh, it looks like xdpyinfo doesn't have what we need - it reports 96x96 on
-- my HiDPI laptop.
-- dpi :: IO (Int, Int)
-- dpi = undefined
