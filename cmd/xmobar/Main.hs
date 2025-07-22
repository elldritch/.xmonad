module Main (main) where

import Relude

import Data.Text (strip)
import System.Directory (doesFileExist, findExecutable)
import System.Process (readProcess)

import Xmobar (
  Config (..),
  Date (..),
  Exec (..),
  Monitors (..),
  Runnable (..),
  StdinReader (..),
  XPosition (..),
  defaultConfig,
  xmobar,
 )

showBattery :: IO Bool
showBattery = getAny . mconcat <$> mapM isBattery batteryIDs
  where
    batteryIDs = ["BAT", "BAT0", "BAT1", "BAT2"]
    isBattery batteryID = do
      let f = "/sys/class/power_supply/" <> batteryID <> "/type"
      exists <- doesFileExist f
      Any
        <$> if exists
          then (== "Battery\n") <$> readFileBS "/sys/class/power_supply/BAT0/type"
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
--
-- ----
--
-- I should write a plugin that takes any `Runnable` and makes it dynamic by
-- wrapping it using a function that returns `Bool` to determine whether to
-- show it or not.
--
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
      { font = "xft:monospace 11"
      , bgColor = "black"
      , fgColor = "silver"
      , position = TopH 30
      , commands =
          [ Run StdinReader
          , Run $ Cpu ["-L", "3", "-H", "70", "--normal", "lime", "--high", "red"] 10
          , Run $ Memory ["-t", "Mem: <usedratio>%", "-L", "5", "-H", "70", "--normal", "lime", "--high", "red"] 10
          , Run $
              DiskU
                [ ("/", "/: <used>/<size> (<usedp>%)")
                , ("/home", "/home: <used>/<size> (<usedp>%)")
                ]
                ["-L", "5", "-H", "70", "--normal", "lime", "--high", "red"]
                10
          , -- Using `Alsa` won't always pick up when the default sink changes
            -- because that plugin runs `alsactl monitor` on the sink directly.
            --
            -- See also: https://github.com/jaor/xmobar/issues/566.
            --
            -- Instead, it seems like this is polling with new `pactl` clients?
            -- To see, run `pactl subscribe` and `forkstat`.
            Run $ Volume "default" "Master" ["--", "--onc", "lime"] 1
          , Run $ Volume "default" "Capture" ["-t", "Mic: <volume>% <status>", "--", "--onc", "lime"] 1
          , -- , Run $
            --     Wireless
            --       ""
            --       [ "--Low"
            --       , "55"
            --       , "--High"
            --       , "80"
            --       , "--low"
            --       , "red"
            --       , "--normal"
            --       , "yellow"
            --       , "--high"
            --       , "lime"
            --       , "-x"
            --       , ""
            --       ]
            --       10
            Run $ Date "%k:%M %a %m/%d/%y" "datetime" 10
          , Run Dunst
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
                  , "lime"
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
          "} %StdinReader%{ "
            ++ intercalate
              " | "
              ( [ "%cpu% %memory% Disk: %disku%"
                , -- , "%default:Master% %default:Capture% %wi%"
                  "%default:Master% %default:Capture%"
                , "%dunst%"
                ]
                  ++ ["%battery%" | batteryPowered]
                  ++ ["%datetime%"]
              )
            ++ " "
      }

data Dunst = Dunst
  deriving stock (Show, Read)

instance Exec Dunst where
  alias :: Dunst -> String
  alias Dunst = "dunst"

  run :: Dunst -> IO String
  run Dunst = do
    isPaused <- strip . toText <$> readProcess "dunstctl" ["is-paused"] ""
    case isPaused of
      "false" -> toString . strip . toText <$> readProcess "dunstctl" ["count", "displayed"] ""
      "true" -> pure "<fc=white>P</fc>"
      _ -> pure "<fc=red>?</fc>"

  rate :: Dunst -> Int
  rate Dunst = 10
