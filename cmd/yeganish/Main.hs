module Main (main) where

import Relude

import Control.Exception (catch)
import Control.Monad.Except (liftEither)
import Data.Map.Lazy qualified as Map
import Data.Text (splitOn)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.SQLite.Simple (FromRow, ToRow, executeMany, execute_, open, query_, withImmediateTransaction)
import Path (Abs, Dir, File, Path, filename, mkRelFile, parent, parseAbsDir, toFilePath, (</>))
import Path.IO (doesPathExist, ensureDir, executable, getHomeDir, getPermissions, listDir)
import System.IO.Error (IOError)
import System.Process (createProcess)
import System.Process qualified as P
import XMWM.Prompt (dmenu')

data ProgramEntry = ProgramEntry
  { name :: String
  , path :: String
  , count :: Int
  , lastUsed :: UTCTime
  }
  deriving (Generic, FromRow, ToRow)

main :: IO ()
main = do
  -- Open SQLite file, creating it if not exists.
  home <- getHomeDir
  let
    dbPath = home </> $(mkRelFile ".local/share/yeganish/cache.db")
    dbPath' = toFilePath dbPath
  dbExists <- doesPathExist dbPath
  db <-
    if dbExists
      then open dbPath'
      else do
        ensureDir $ parent dbPath
        db <- open dbPath'
        execute_ db "CREATE TABLE IF NOT EXISTS program (name TEXT NOT NULL UNIQUE, path TEXT NOT NULL, count INTEGER NOT NULL, last_used TEXT NOT NULL)"
        pure db

  -- Read saved program entries and invocation counts from SQLite, sorting by
  -- frecency. Frecency is like frequency, but also applies a penalty for
  -- entries that have not been used for a long time.
  --
  -- In particular, note how _score_ is calculated in this query. To tune the
  -- scoring behavior, adjust the penalty decay factor (which must be < 1 in
  -- order to ensure that elapsed time _reduces_ score) and the scaling on the
  -- elapsed time (currently at _minutes_ but could be scaled to hours or days).
  entries <-
    query_ @ProgramEntry
      db
      "SELECT name, path, count, last_used FROM \
      \ (SELECT name, path, count, last_used, \
      \  1 + count * POWER(0.95, (UNIXEPOCH('now') - UNIXEPOCH(last_used)) / 60) AS score \
      \  FROM program ORDER BY score DESC) t"

  -- Present selection to user.
  selected <- runExceptT $ dmenu' (.name) entries

  -- If selected, run program.
  selected' <- case selected of
    Left e -> putStrLn e $> Nothing
    Right program -> do
      void $ createProcess $ P.proc program.path []
      pure $ Just program

  -- Scan for new programs in $PATH.
  pathBins <- resolvePathBins
  scanned <- case pathBins of
    Left e -> putStrLn e *> exitFailure
    Right bins -> pure bins

  -- Update SQLite with new programs and invocation counts.
  now <- getCurrentTime
  let
    -- First, look up each existing entry in the scanned map by name, plucking
    -- entries out of the scanned map on examination. We use this to update any
    -- scanned paths and remove any entries that no longer exist.
    (entries', scanned') = foldr updateEntry ([], scanned) entries

    -- All remaining entries are new, so create program entry rows for them.
    entries'' = entries' ++ map (\(k, v) -> ProgramEntry{name = k, path = toFilePath v, count = 0, lastUsed = now}) (Map.toList scanned')

    -- Finally, update the count and last used timestamp for the program that
    -- was just selected, if a program was selected.
    entries''' = case selected' of
      Just s -> map (\e -> if e.name == s.name then s{count = e.count + 1, lastUsed = now} else e) entries''
      Nothing -> entries''

  -- We just clear the whole database and rewrite every row.
  withImmediateTransaction db $ do
    execute_ db "DELETE FROM program"
    executeMany db "INSERT INTO program (name, path, count, last_used) VALUES (?, ?, ?, ?)" entries'''
  pass
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

    updateEntry :: ProgramEntry -> ([ProgramEntry], Map String (Path Abs File)) -> ([ProgramEntry], Map String (Path Abs File))
    updateEntry entry (xs, scanned) = case Map.lookup entry.name scanned of
      Just scannedPath -> (entry{path = toFilePath scannedPath} : xs, Map.delete entry.name scanned)
      Nothing -> (xs, scanned)
