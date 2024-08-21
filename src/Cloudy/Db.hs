{-# LANGUAGE DeriveAnyClass #-}

module Cloudy.Db where

import Database.SQLite.Simple (withConnection, Connection, execute_, Query, query_, FromRow)
import Cloudy.Path (getCloudyDbPath)
import Control.Exception (Exception, throwIO)

createLocalDatabase :: Connection -> IO ()
createLocalDatabase conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS cloudy_instance \
    \  ( id INTEGER PRIMARY KEY AUTOINCREMENT \
    \  , name TEXT NOT NULL \
    \  , created_at DATETIME NOT NULL DEFAULT (datetime('now', 'utc')) \
    \  , deleted_at DATETIME \
    \  ) \
    \STRICT; \
    \\
    \CREATE TABLE IF NOT EXISTS scaleway_instance \
    \  ( cloudy_instance_id INTEGER NOT NULL \
    \  , scaleway_instance_id TEXT NOT NULL UNIQUE \
    \  , FOREIGN KEY (cloudy_instance_id) REFERENCES cloudy_instance(id) \
    \  ) \
    \STRICT;"

withCloudyDb :: (Connection -> IO ()) -> IO ()
withCloudyDb action = do
  dcutDbPath <- getCloudyDbPath
  withSqliteConn dcutDbPath action

withSqliteConn :: FilePath -> (Connection -> IO ()) -> IO ()
withSqliteConn dbPath action =
  withConnection
      dbPath
      (\conn -> do
        execute_
          conn
          -- Also consider using the following settings:
          --
          -- - `PRAGMA synchronous = NORMAL;`:
          --     Change the synchronization model.  NORMAL is faster than the
          --     default, and still safe with journal_mode=WAL.
          --
          -- - `PRAGMA cache_size = 1000000000;`:
          --     Change the maximum number of database disk pages that SQLite
          --     will hold in memory at once. Each page uses about 1.5K of
          --     memory. The default cache size is 2000.
          --
          -- More suggestions in https://kerkour.com/sqlite-for-servers
          "PRAGMA journal_mode = WAL; -- better concurrency \
          \PRAGMA foreign_keys = true; -- enforce foreign key constraints \
          \PRAGMA busy_timeout = 5000; -- helps prevent SQLITE_BUSY errors"
        action conn
      )

data QuerySingleErr = QuerySingleErr Query String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

querySingle_ :: FromRow a => Connection -> Query -> IO a
querySingle_ conn q = do
  res <- query_ conn q
  case res of
    [] -> throwIO $ QuerySingleErr q "query returned NO results, expecting exactly one"
    [a] -> pure a
    _:_ -> throwIO $ QuerySingleErr q "query returned multiple results, expecting exactly one"
