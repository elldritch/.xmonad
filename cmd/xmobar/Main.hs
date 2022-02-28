module Main (main) where

import Relude

import System.Directory (doesFileExist, findExecutable)
import System.Process (readProcess)

import Xmobar

showBattery :: IO Bool
showBattery = getAny . mconcat <$> mapM isBattery batteryIDs
  where
    batteryIDs = ["BAT", "BAT0", "BAT1", "BAT2"]
    isBattery batteryID = do
      let f = "/sys/class/power_supply/" <> batteryID <> "/type"
      exists <- doesFileExist f
      Any
        <$> if exists
          then (== "Battery") <$> readFile "/sys/class/power_supply/BAT0/type"
          else pure False

-- Show if:
--
-- 1. WiFi is connected.
-- 2. WiFi is disconnected and there's no ethernet connection.
--
-- ----
--
-- TODO: How do I get the wireless indicator to show up _dynamically_? What if
-- I change connections? I need the wireless indicator to change as my network
-- status changes.
--
-- Maybe I need to make a proper `Monitor`? I basically want a combination of
-- `DynNetwork` and `Wireless`.
--
-- ----
--
-- TODO: Notes on a more portable implementation:
--
-- Checking whether WiFi is connected:
--
-- 1. Check `/sys/class/net` for device with `/wireless`.
-- 2. Run `iw dev DEVICE link`, check whether output is "Not connected."
--
-- Checking whether ethernet is connected:
--
-- 1. TODO: How do I get a list of ethernet devices?
-- 2. `cat /sys/class/net/DEVICE/operstate` == `up`.
showWirelessInterface :: IO Bool
showWirelessInterface = do
  hasNMCLI <- findExecutable "nmcli"
  case hasNMCLI of
    Just nmcli -> do
      out <- readProcess nmcli ["device", "status"] ""
      let lexed = words <$> lines (toText out)
      let eth = connectedEthernet lexed
      let wifi = connectedWiFi lexed
      pure $ case wifi of
        Just _ -> True
        Nothing -> case eth of
          Just _ -> False
          Nothing -> True
    Nothing -> pure True
  where
    connectedEthernet :: [[Text]] -> Maybe Text
    connectedEthernet lexed = asum $ line <$> lexed
      where
        line [_, "ethernet", "connected", connection] = Just connection
        line _ = Nothing

    connectedWiFi :: [[Text]] -> Maybe Text
    connectedWiFi lexed = asum $ line <$> lexed
      where
        line [_, "wifi", "connected", connection] = Just connection
        line _ = Nothing

main :: IO ()
main = do
  batteryPowered <- showBattery
  xmobar $
    defaultConfig
      { font = "xft:monospace"
      , bgColor = "black"
      , fgColor = "grey"
      , position = TopW L 100
      , commands =
          [ Run StdinReader
          , Run $ Cpu ["-L", "3", "-H", "70", "--normal", "green", "--high", "red"] 10
          , Run $ Memory ["-t", "Mem: <usedratio>%", "-L", "5", "-H", "70", "--normal", "green", "--high", "red"] 10
          , Run $
              DiskU
                [ ("/", "/: <used>/<size> (<usedp>%)")
                , ("/home", "/home: <used>/<size> (<usedp>%)")
                ]
                ["-L", "5", "-H", "70", "--normal", "green", "--high", "red"]
                10
          , Run $ Volume "default" "Master" [] 1 -- Using `Alsa` won't always pick up when the default sink changes.
          , Run $ Alsa "default" "Capture" ["-t", "Mic: <volume>% <status>"]
          , Run $
              Wireless
                ""
                [ "--Low"
                , "55"
                , "--High"
                , "80"
                , "--low"
                , "red"
                , "--normal"
                , "yellow"
                , "--high"
                , "green"
                , "-x"
                , "-"
                ]
                10
          , Run $ Date "%k:%M %a %m/%d/%y" "datetime" 10
          ]
            ++ [ Run $
                Battery
                  [ "--template"
                  , "<left>%<acstatus>, <timeleft> left"
                  , "--Low"
                  , "40"
                  , "--High"
                  , "80"
                  , "--low"
                  , "red"
                  , "--normal"
                  , "yellow"
                  , "--high"
                  , "green"
                  , "--"
                  , "-O"
                  , " (Charging)"
                  , "-i"
                  , " (Charged)"
                  , "-o"
                  , ""
                  ]
                  60
               | batteryPowered
               ]
      , sepChar = "%"
      , alignSep = "}{"
      , template =
          " %StdinReader% }{ "
            ++ intercalate
              " | "
              ( [ "%cpu% %memory% Disk: %disku%"
                , "%default:Master% %alsa:default:Capture%"
                , "%wi%"
                ]
                  ++ ["%battery%" | batteryPowered]
                  ++ ["%datetime%"]
              )
            ++ " "
      }
