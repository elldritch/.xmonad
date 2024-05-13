module Main (main) where

import Relude

import Data.Default (def)
import System.Taffybar.Information.CPU (cpuLoad)
import System.Taffybar.SimpleConfig (
  SimpleTaffyConfig (..),
  simpleTaffybar,
 )
import System.Taffybar.Widget (
  WorkspacesConfig (..),
  textClockNewWith,
  workspacesNew, hideEmpty,
 )
import System.Taffybar.Widget.Generic.Graph (
  GraphConfig (..),
 )
import System.Taffybar.Widget.Generic.PollingGraph (
  pollingGraphNew,
 )

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO ()
main = do
  simpleTaffybar $
    def
      { monitorsAction = pure [0]
      , startWidgets = [workspaces]
      , endWidgets = [clock, cpu]
      }
  where
    cpuCfg =
      def
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
        , graphLabel = Just "cpu"
        }
    clock = textClockNewWith def
    cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
    workspaces =
      workspacesNew
        def
          { maxIcons = Just 0
          , showWorkspaceFn = hideEmpty
          }
