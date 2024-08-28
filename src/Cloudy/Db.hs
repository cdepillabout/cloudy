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
import Data.Void (Void)

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
    \  , scaleway_zone TEXT NOT NULL \
    \  , scaleway_instance_id TEXT NOT NULL UNIQUE \
    \  , scaleway_ip_id TEXT NOT NULL UNIQUE \
    \  , scaleway_ip_address TEXT NOT NULL \
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

querySingleErr_ :: FromRow a => Connection -> Query -> IO a
querySingleErr_ conn q = do
  res <- query_ conn q
  case res of
    [] -> throwIO $ QuerySingleErr q "query returned NO results, expecting exactly one"
    [a] -> pure a
    _:_ -> throwIO $ QuerySingleErr q "query returned multiple results, expecting exactly one"

data OnlyOne a = OnlyOne a | MultipleExist | NoneExist
  deriving stock (Functor, Show)

querySingle_ :: FromRow a => Connection -> Query -> IO (OnlyOne a)
querySingle_ conn q = do
  res <- query_ conn q
  case res of
    [] -> pure NoneExist
    [a] -> pure $ OnlyOne a
    _:_ -> pure MultipleExist

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
  , scalewayZone :: Text
  , scalewayInstanceId :: Text
  , scalewayIpId :: Text
  , scalewayIpAddress :: Text
  }
  deriving stock (Eq, Show)

instance FromRow ScalewayInstance where
  fromRow = do
    cloudyInstanceId <- field
    scalewayZone <- field
    scalewayInstanceId <- field
    scalewayIpId <- field
    scalewayIpAddress <- field
    pure $ ScalewayInstance { cloudyInstanceId, scalewayZone, scalewayInstanceId, scalewayIpId, scalewayIpAddress }

instance ToRow ScalewayInstance where
  toRow ScalewayInstance {cloudyInstanceId, scalewayZone, scalewayInstanceId, scalewayIpId, scalewayIpAddress} =
    toRow (cloudyInstanceId, scalewayZone, scalewayInstanceId, scalewayIpId, scalewayIpAddress)

data InstanceInfo
  = CloudyScalewayInstance (CloudyInstance, ScalewayInstance)
  | CloudyAwsInstance (CloudyInstance, Void {- TODO: actually implement AWS stuff -})
  deriving stock Show

newCloudyInstance :: Connection -> IO (CloudyInstanceId, Text)
newCloudyInstance conn = withTransaction conn go
  where
    go :: IO (CloudyInstanceId, Text)
    go = do
      possibleName <- instanceNameGen
      maybeInstance <- findCloudyInstanceByNameWithDeleted conn possibleName
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

-- | Return a cloudy instance matching the given name.
-- This will return an instance even if it has already been deleted.
findCloudyInstanceByNameWithDeleted :: Connection -> Text -> IO (Maybe CloudyInstance)
findCloudyInstanceByNameWithDeleted conn cloudyInstanceName = do
  listToMaybe <$>
    query
      conn
      "SELECT id, name, created_at, deleted_at \
      \FROM cloudy_instance \
      \WHERE name == ? \
      \ORDER BY id"
      (Only cloudyInstanceName)

findCloudyInstanceIdByName :: Connection -> Text -> IO (Maybe CloudyInstanceId)
findCloudyInstanceIdByName conn cloudyInstanceName = do
  fmap fromOnly . listToMaybe <$>
    query
      conn
      "SELECT id \
      \FROM cloudy_instance \
      \WHERE name == ? AND deleted_at IS NOT NULL"
      (Only cloudyInstanceName)

newScalewayInstance ::
  Connection ->
  UTCTime ->
  CloudyInstanceId ->
  -- | Scaleway Zone
  Text ->
  -- | Scaleway Instance Id
  Text ->
  -- | Scaleway IP Id
  Text ->
  -- | Scaleway IP Address
  Text ->
  IO ()
newScalewayInstance conn creationTime cloudyInstanceId scalewayZone scalewayInstanceId scalewayIpId scalewayIpAddress =
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
      \(cloudy_instance_id, scaleway_zone scaleway_instance_id, scaleway_ip_id, scaleway_ip_address) \
      \VALUES (?, ?, ?, ?, ?)"
      ScalewayInstance { cloudyInstanceId, scalewayZone, scalewayInstanceId, scalewayIpId, scalewayIpAddress }

-- | Return a single CloudyInstanceId if there is exactly one in the database that
-- is not already deleted.
findOnlyOneInstanceId :: Connection -> IO (OnlyOne CloudyInstanceId)
findOnlyOneInstanceId conn = do
  onlyOneInstId <-
    querySingle_
      conn
      "SELECT id \
      \FROM cloudy_instance \
      \WHERE deleted_at IS NOT NULL"
  pure $ fmap fromOnly onlyOneInstId

utcTimeToSqliteInt :: UTCTime -> Int64
utcTimeToSqliteInt = round . utcTimeToPOSIXSeconds

assertDbInvariants :: Connection -> IO ()
assertDbInvariants conn = undefined

instanceInfoForId :: Connection -> CloudyInstanceId -> IO (Maybe InstanceInfo)
instanceInfoForId conn cloudyInstanceId = do
