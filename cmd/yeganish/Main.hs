module Main (main) where

import Relude

import Control.Exception (catch)
import Control.Monad.Except (liftEither)
import Data.Map.Lazy qualified as Map
import Data.Text (splitOn)
import Data.Text qualified as T
import Path (Abs, Dir, File, Path, filename, parseAbsDir, toFilePath)
import Path.IO (executable, getPermissions, listDir)
import System.IO.Error (IOError)
import System.Process (createProcess)
import System.Process qualified as P
import XMWM.Prompt (dmenu')

main :: IO ()
main = do
  result <- runExceptT $ do
    pathBins <- liftEither =<< liftIO resolvePathBins
    snd <$> dmenu' fst (Map.toList pathBins)
  case result of
    Left e -> putStrLn e *> exitFailure
    Right bin -> void $ createProcess $ P.proc (toFilePath bin) []
  where
    resolvePathBins :: IO (Either String (Map String (Path Abs File)))
    resolvePathBins = runExceptT $ do
      path <- lookupEnv "PATH"
      path' <- liftEither $ maybeToRight "$PATH not set" path
      path'' <- liftIO $ warnUnparseablePaths $ parsePathToDirs path'
      bins <- concat <$> traverse (liftIO . binsInDir) path''
      pure $ foldl' resolveBin mempty bins
      where
        resolveBin :: Map String (Path Abs File) -> Path Abs File -> Map String (Path Abs File)
        resolveBin resolved bin =
          if Map.member name resolved
            then resolved
            else Map.insert name bin resolved
          where
            name :: String
            name = toFilePath $ filename bin

    -- TODO: Handle escape characters.
    parsePathToDirs :: String -> [Either SomeException (Path Abs Dir)]
    parsePathToDirs = map parseAbsDir . splitOnColon
      where
        splitOnColon :: String -> [String]
        splitOnColon = map T.unpack . splitOn ":" . T.pack

    warnUnparseablePaths :: [Either SomeException (Path Abs Dir)] -> IO [Path Abs Dir]
    warnUnparseablePaths = do
      (catMaybes <$>)
        . traverse
          ( \case
              Left e -> do
                putStrLn $ "Could not parse $PATH element: " <> displayException e
                pure Nothing
              Right p -> pure (Just p)
          )

    binsInDir :: Path Abs Dir -> IO [Path Abs File]
    binsInDir dir = do
      (_dirs, files) <- listDir dir `catch` \(_ :: IOError) -> pure ([], [])
      catMaybes <$> traverse executables files
      where
        executables :: Path Abs File -> IO (Maybe (Path Abs File))
        executables file = do
          permissions <- getPermissions file
          pure $ if executable permissions then Just file else Nothing
