module Main (main) where

import Relude

import Control.Exception (catch)
import Data.Text (splitOn)
import Data.Text qualified as T
import System.Directory (Permissions (executable), getPermissions, listDirectory)
import System.IO.Error (IOError)

main :: IO ()
main = do
  path <-
    lookupEnv "PATH" >>= \case
      Nothing -> do
        putStrLn "PATH not set"
        exitFailure
      Just p -> pure p
  let paths = parsePath path
  bins <- traverse binsInDir paths
  print $ concat bins
  where
    -- TODO: Handle escape characters.
    parsePath :: String -> [FilePath]
    parsePath p = map T.unpack $ splitOn ":" (T.pack p)

    binsInDir :: FilePath -> IO [(String, FilePath)]
    binsInDir dir = do
      files <- listDirectory dir `catch` \(_ :: IOError) -> pure []
      isBins <- traverse (isBin dir) files
      pure $ map fst $ filter snd $ zip (zip files (map (\f -> dir <> "/" <> f) files)) isBins

    isBin :: FilePath -> FilePath -> IO Bool
    isBin dir file = do
      permissions <- getPermissions (dir <> "/" <> file)
      pure $ executable permissions
