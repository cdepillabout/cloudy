{-# LANGUAGE DeriveAnyClass #-}

module Cloudy.Db where

import Cloudy.NameGen (instanceNameGen)
import Cloudy.Path (getCloudyDbPath)
import Control.Exception (Exception, throwIO)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (withConnection, Connection, execute_, Query, query_, FromRow (..), ToRow (..), execute, withTransaction, lastInsertRowId, query, Only (..), field)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

createLocalDatabase :: Connection -> IO ()
createLocalDatabase conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS cloudy_instance \
    \  ( id INTEGER PRIMARY KEY AUTOINCREMENT \
    \  , name TEXT NOT NULL UNIQUE \
    \  , created_at INTEGER \
    \  , deleted_at INTEGER \
    \  ) \
    \STRICT"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS scaleway_instance \
    \  ( cloudy_instance_id INTEGER NOT NULL UNIQUE \
    \  , scaleway_instance_id TEXT NOT NULL UNIQUE \
    \  , FOREIGN KEY (cloudy_instance_id) REFERENCES cloudy_instance(id) \
    \  ) \
    \STRICT"

withCloudyDb :: (Connection -> IO ()) -> IO ()
withCloudyDb action = do
  dcutDbPath <- getCloudyDbPath
  withSqliteConn dcutDbPath $ \conn -> do
    createLocalDatabase conn
    action conn

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

newtype CloudyInstanceId = CloudyInstanceId { unCloudyInstanceId :: Int64 }
  deriving stock (Eq, Show)
  deriving newtype (FromField, ToField)

data CloudyInstance = CloudyInstance
  { id :: CloudyInstanceId
  , name :: Text
  , createdAt :: Maybe UTCTime
  , deletedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

instance FromRow CloudyInstance where
  fromRow = do
    id' <- field
    name <- field
    createdAt <- field
    deletedAt <- field
    pure $ CloudyInstance { id = id', name, createdAt, deletedAt }

data ScalewayInstance = ScalewayInstance
  { cloudyInstanceId :: CloudyInstanceId
  , scalewayInstanceId :: Text
  }
  deriving stock (Eq, Show)

instance FromRow ScalewayInstance where
  fromRow = do
    cloudyInstanceId <- field
    scalewayInstanceId <- field
    pure $ ScalewayInstance { cloudyInstanceId, scalewayInstanceId }

instance ToRow ScalewayInstance where
  toRow ScalewayInstance {cloudyInstanceId, scalewayInstanceId} =
    toRow (cloudyInstanceId, scalewayInstanceId)

newCloudyInstance :: Connection -> IO (CloudyInstanceId, Text)
newCloudyInstance conn = withTransaction conn go
  where
    go :: IO (CloudyInstanceId, Text)
    go = do
      possibleName <- instanceNameGen
      maybeInstance <- findCloudyInstanceByName conn possibleName
      case maybeInstance of
        -- No instance exists with this name yet. Insert a new blank instance.
        Nothing -> do
          execute
            conn
            "INSERT INTO cloudy_instance \
            \(name) \
            \VALUES (?)"
            (Only possibleName)
          cloudyInstanceId <- lastInsertRowId conn
          pure (CloudyInstanceId cloudyInstanceId, possibleName)
        -- An instance already exists with this name, try again.
        Just _ -> go

findCloudyInstanceByName :: Connection -> Text -> IO (Maybe CloudyInstance)
findCloudyInstanceByName conn cloudyInstanceName = do
  listToMaybe <$>
    query
      conn
      "SELECT id, name, created_at, deleted_at \
      \FROM cloudy_instance \
      \WHERE name == ? \
      \ORDER BY id"
      (Only cloudyInstanceName)

newScalewayInstance ::
  Connection -> UTCTime -> CloudyInstanceId -> Text -> IO ()
newScalewayInstance conn creationTime cloudyInstanceId scalewayInstanceId =
  withTransaction conn $ do
    execute
      conn
      "UPDATE cloudy_instance \
      \SET created_at = ? \
      \WHERE id = ?"
      (utcTimeToSqliteInt creationTime, cloudyInstanceId)
    execute
      conn
      "INSERT INTO scaleway_instance \
      \(cloudy_instance_id, scaleway_instance_id) \
      \VALUES (?, ?)"
      ScalewayInstance { cloudyInstanceId, scalewayInstanceId }

utcTimeToSqliteInt :: UTCTime -> Int64
utcTimeToSqliteInt = round . utcTimeToPOSIXSeconds

-- insertScalewayServer :: Connection -> Scaleway.ServerId -> Text -> IO ()
-- insertScalewayServer conn scalewayServerId serverName = withTransaction $ do
--   execute
--     conn
--     "INSERT INTO cloudy_instance \
--     \(name) \
--     \VALUES (?)"
--     serverName

-- insertLocalObservation :: Connection -> LocalObservation -> IO ()
-- insertLocalObservation conn =
--   execute
--     conn
--     "INSERT INTO idle_times \
--     \(client_uuid, timestamp, idle_time_milli, hostname) \
--     \VALUES (?,?,?,?)"

-- insertNewObsFromServer :: Connection -> [ObsFromServer] -> IO ()
-- insertNewObsFromServer conn obsFromServer =
--   executeMany
--     conn
--     -- TODO: is ignoring a conflict the best thing to do here???
--     -- Or maybe I need to replace it?
--     -- It looks like SQLite has upsert functionality as well??
--     -- https://www.sqlite.org/lang_upsert.html
--     "INSERT OR IGNORE INTO idle_times \
--     \(server_id, client_uuid, timestamp, idle_time_milli, hostname) \
--     \VALUES (?,?,?,?,?)"
--     (fmap toDb obsFromServer)
--   where
--     toDb (ObsFromServer servId uuid ts idle host) = (servId, UUID.toText uuid, ts, idle, host)