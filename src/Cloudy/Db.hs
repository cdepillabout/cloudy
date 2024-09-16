{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Db where

import Cloudy.InstanceSetup.Types (InstanceSetup (..))
import Cloudy.NameGen (instanceNameGen)
import Cloudy.Path (getCloudyDbPath)
import Control.Exception (Exception, throwIO)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as ByteString
import Data.Foldable (fold)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time ( UTCTime, getCurrentTime )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Traversable (for)
import Data.Void (Void)
import Database.SQLite.Simple (withConnection, Connection, execute_, Query, query_, FromRow (..), ToRow (..), execute, withTransaction, lastInsertRowId, query, Only (..), field)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

createLocalDatabase :: Connection -> IO ()
createLocalDatabase conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS cloudy_instance \
    \  ( id INTEGER PRIMARY KEY AUTOINCREMENT \
    \  , name TEXT NOT NULL UNIQUE \
    \  , created_at INTEGER \
    \  , deleted_at INTEGER \
    \  , instance_setup TEXT \
    \  ) \
    \STRICT"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS scaleway_instance \
    \  ( cloudy_instance_id INTEGER NOT NULL UNIQUE \
    \  , scaleway_zone TEXT NOT NULL \
    \  , scaleway_instance_id TEXT NOT NULL UNIQUE \
    \  , scaleway_ip_id TEXT NOT NULL \
    \  , scaleway_ip_address TEXT NOT NULL \
    \  , FOREIGN KEY (cloudy_instance_id) REFERENCES cloudy_instance(id) \
    \  ) \
    \STRICT"

withCloudyDb :: (Connection -> IO a) -> IO a
withCloudyDb action = do
  dcutDbPath <- getCloudyDbPath
  withSqliteConn dcutDbPath $ \conn -> do
    createLocalDatabase conn
    -- TODO: Maybe create some sort of production build that doesn't check
    -- the invariants.
    assertDbInvariants conn
    res <- action conn
    assertDbInvariants conn
    pure res

withSqliteConn :: FilePath -> (Connection -> IO a) -> IO a
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

querySingleErr_ :: FromRow r => Connection -> Query -> IO r
querySingleErr_ conn q = do
  onlyOneRes <- querySingle_ conn q
  case onlyOneRes of
    OnlyOne r -> pure r
    NoneExist -> throwIO $ QuerySingleErr q "query returned NO results, expecting exactly one"
    MultipleExist -> throwIO $ QuerySingleErr q "query returned multiple results, expecting exactly one"

data OnlyOne r = OnlyOne r | MultipleExist | NoneExist
  deriving stock (Functor, Show)

querySingle_ :: FromRow r => Connection -> Query -> IO (OnlyOne r)
querySingle_ conn q = do
  res <- query_ conn q
  case res of
    [] -> pure NoneExist
    [r] -> pure $ OnlyOne r
    _:_ -> pure MultipleExist


-- | Query on a column with a UNIQUE constraint.  Throws an exception if
-- multiple values are returned.
queryUnique :: (ToRow a, FromRow r) => Connection -> Query -> a -> IO (Maybe r)
queryUnique conn q a = do
  res <- query conn q a
  case res of
    [] -> pure Nothing
    [r] -> pure $ Just r
    _:_ -> error "queryUnique: expecting only a single result at most, but got multiple results.  Is there really a UNIQUE constraint here?"

newtype CloudyInstanceId = CloudyInstanceId { unCloudyInstanceId :: Int64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromField, ToField)

data CloudyInstance = CloudyInstance
  { id :: CloudyInstanceId
  , name :: Text
  , createdAt :: Maybe UTCTime
  , deletedAt :: Maybe UTCTime
  , instanceSetup :: Maybe InstanceSetup
  }
  deriving stock (Eq, Show)

instance FromRow CloudyInstance where
  fromRow = do
    id' <- field
    name <- field
    createdAt <- fmap utcTimeFromSqliteInt <$> field
    deletedAt <- fmap utcTimeFromSqliteInt <$> field
    instanceSetup <- fmap unDbInstanceSetup <$> field
    pure $ CloudyInstance { id = id', name, createdAt, deletedAt, instanceSetup }

-- | newtype to hold the 'FromField' instance for 'InstanceSetup', for use in
-- the 'FromRow' instance for 'CloudyInstance'.
--
-- The @instance_setup@ column in the @cloudy_instance@ table holds a
-- JSON-encoded 'InstanceSetup' value.
newtype DbInstanceSetup = DbInstanceSetup { unDbInstanceSetup :: InstanceSetup }

instance FromField DbInstanceSetup where
  fromField fld = do
    rawInstanceSetup :: Text <- fromField fld
    let eitherInstanceSetup = eitherDecodeStrict $ encodeUtf8 rawInstanceSetup
    case eitherInstanceSetup of
      Left err -> fail $ "Failed to json decode instance_setup column as InstanceSetup: " <> err
      Right instanceSetup -> pure $ DbInstanceSetup instanceSetup

instance ToField DbInstanceSetup where
  toField (DbInstanceSetup instSetup) =
    toField $ decodeUtf8 $ ByteString.toStrict $ encode instSetup

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
  = CloudyScalewayInstance CloudyInstance ScalewayInstance
  | CloudyAwsInstance CloudyInstance Void {- TODO: actually implement AWS stuff -}
  deriving stock Show

cloudyInstanceFromInstanceInfo :: InstanceInfo -> CloudyInstance
cloudyInstanceFromInstanceInfo = \case
  CloudyScalewayInstance cloudyInstance _ -> cloudyInstance
  CloudyAwsInstance cloudyInstance _ -> cloudyInstance

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
      "SELECT id, name, created_at, deleted_at, instance_setup \
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
      \WHERE name == ? AND deleted_at IS NULL"
      (Only cloudyInstanceName)

findCloudyInstanceById :: Connection -> CloudyInstanceId -> IO (Maybe CloudyInstance)
findCloudyInstanceById conn cloudyInstanceId = do
  listToMaybe <$>
    query
      conn
      "SELECT id, name, created_at, deleted_at, instance_setup \
      \FROM cloudy_instance \
      \WHERE id == ? AND deleted_at IS NULL AND created_at IS NOT NULL"
      (Only cloudyInstanceId)

findAllCloudyInstances :: Connection -> IO [CloudyInstance]
findAllCloudyInstances conn =
  query_
    conn
    "SELECT id, name, created_at, deleted_at, instance_setup \
    \FROM cloudy_instance \
    \WHERE deleted_at IS NULL AND created_at IS NOT NULL"

setCloudyInstanceDeleted :: Connection -> CloudyInstanceId -> IO ()
setCloudyInstanceDeleted conn cloudyInstanceId = do
    currTime <- getCurrentTime
    execute
      conn
      "UPDATE cloudy_instance \
      \SET deleted_at = ? \
      \WHERE id = ?"
      (utcTimeToSqliteInt currTime, cloudyInstanceId)

newScalewayInstance ::
  Connection ->
  UTCTime ->
  CloudyInstanceId ->
  Maybe InstanceSetup ->
  -- | Scaleway Zone
  Text ->
  -- | Scaleway Instance Id
  Text ->
  -- | Scaleway IP Id
  Text ->
  -- | Scaleway IP Address
  Text ->
  IO ()
newScalewayInstance conn creationTime cloudyInstanceId instSetup scalewayZone scalewayInstanceId scalewayIpId scalewayIpAddress =
  withTransaction conn $ do
    execute
      conn
      "UPDATE cloudy_instance \
      \SET created_at = ?, instance_setup = ? \
      \WHERE id = ?"
      (utcTimeToSqliteInt creationTime, fmap DbInstanceSetup instSetup, cloudyInstanceId)
    execute
      conn
      "INSERT INTO scaleway_instance \
      \(cloudy_instance_id, scaleway_zone, scaleway_instance_id, scaleway_ip_id, scaleway_ip_address) \
      \VALUES (?, ?, ?, ?, ?)"
      ScalewayInstance { cloudyInstanceId, scalewayZone, scalewayInstanceId, scalewayIpId, scalewayIpAddress }

findScalewayInstanceByCloudyInstanceId :: Connection -> CloudyInstanceId -> IO (Maybe ScalewayInstance)
findScalewayInstanceByCloudyInstanceId conn cloudyInstanceId =
  listToMaybe <$>
    query
      conn
      "SELECT cloudy_instance_id, scaleway_zone, scaleway_instance_id, scaleway_ip_id, scaleway_ip_address \
      \FROM scaleway_instance \
      \WHERE cloudy_instance_id == ?"
      (Only cloudyInstanceId)

findAllScalewayInstances :: Connection -> IO [ScalewayInstance]
findAllScalewayInstances conn =
  query_
    conn
    "SELECT \
    \  scaleway_instance.cloudy_instance_id, \
    \  scaleway_instance.scaleway_zone, \
    \  scaleway_instance.scaleway_instance_id, \
    \  scaleway_instance.scaleway_ip_id, \
    \  scaleway_instance.scaleway_ip_address \
    \FROM scaleway_instance \
    \INNER JOIN cloudy_instance ON scaleway_instance.cloudy_instance_id == cloudy_instance.id \
    \WHERE cloudy_instance.deleted_at IS NULL AND cloudy_instance.created_at IS NOT NULL"


-- | Return a single CloudyInstanceId if there is exactly one in the database that
-- is not already deleted.
findOnlyOneInstanceId :: Connection -> IO (OnlyOne CloudyInstanceId)
findOnlyOneInstanceId conn = do
  onlyOneInstId <-
    querySingle_
      conn
      "SELECT id \
      \FROM cloudy_instance \
      \WHERE deleted_at IS NULL"
  pure $ fmap fromOnly onlyOneInstId

utcTimeToSqliteInt :: UTCTime -> Int64
utcTimeToSqliteInt = round . utcTimeToPOSIXSeconds

utcTimeFromSqliteInt :: Int64 -> UTCTime
utcTimeFromSqliteInt = posixSecondsToUTCTime . fromIntegral

instanceInfoForId :: Connection -> CloudyInstanceId -> IO (Maybe InstanceInfo)
instanceInfoForId conn cloudyInstanceId = withTransaction conn $ do
  maybeCloudyInstance <- findCloudyInstanceById conn cloudyInstanceId
  case maybeCloudyInstance of
    Nothing -> pure Nothing
    Just cloudyInstance -> do
      maybeCloudyInstanceId <- findScalewayInstanceByCloudyInstanceId conn cloudyInstance.id
      pure $ fmap (CloudyScalewayInstance cloudyInstance) maybeCloudyInstanceId

findAllInstanceInfos :: Connection -> IO [InstanceInfo]
findAllInstanceInfos conn = withTransaction conn $ do
  cloudyInstances <- findAllCloudyInstances conn
  scalewayInstances <- findAllScalewayInstances conn
  for cloudyInstances $ \cloudyInstance -> do
    let maybeScalewayInstance =
          find
            (\scalewayInst -> scalewayInst.cloudyInstanceId == cloudyInstance.id)
            scalewayInstances
    case maybeScalewayInstance of
      Nothing ->
        error $ "Could not find scaleway instance for cloudyInstance: " <> show cloudyInstance
      Just scalewayInstance ->
        pure $ CloudyScalewayInstance cloudyInstance scalewayInstance


data DbInvariantErr
  = CloudyInstanceHasNoProviderInstance CloudyInstanceId
  | CloudyInstanceHasMultipleProviderInstances CloudyInstanceId
  | CloudyInstanceHasNullCreatedAt CloudyInstanceId
  deriving stock Show

assertDbInvariants :: Connection -> IO ()
assertDbInvariants conn = withTransaction conn $ do
  invariantErrors :: [DbInvariantErr] <-
    fold
      [ invariantEveryCloudyInstHasExactlyOneProviderInst conn
      , invariantCloudyInstCorectDates conn
      -- TODO: add invariant that says two non-deleted scaleway servers should
      -- never have the same IP addresses
      ]
  case invariantErrors of
    [] -> pure ()
    _ ->
      error $
        "assertDbInvariants: DB invariants have been violated: " <> show invariantErrors

-- | There needs to be EXACTLY ONE corresponding cloud provider instance for each
-- cloudy instance.
invariantEveryCloudyInstHasExactlyOneProviderInst :: Connection -> IO [DbInvariantErr]
invariantEveryCloudyInstHasExactlyOneProviderInst conn = do
  allCloudyInstIds <- fmap fromOnly <$> query_ conn "SELECT id FROM cloudy_instance"
  maybeErrs <- for allCloudyInstIds checkCloudyInstProviders
  pure $ catMaybes maybeErrs
  where
    checkCloudyInstProviders :: CloudyInstanceId -> IO (Maybe DbInvariantErr)
    checkCloudyInstProviders cloudyInstId = do
      maybeScalewayInstId :: Maybe Text <-
        fmap fromOnly <$>
          queryUnique
            conn
            "SELECT scaleway_instance_id \
            \FROM scaleway_instance \
            \WHERE cloudy_instance_id == ?"
            (Only cloudyInstId)
      case maybeScalewayInstId of
        Just _scalewayInstId -> pure Nothing
        Nothing -> pure $ Just $ CloudyInstanceHasNoProviderInstance cloudyInstId

-- | Cloudy instances should always have a @created_at@ value that is non-null.
--
-- The only time a Cloudy instance can have a @created_at@ value that is null
-- is within the Create CLI command.  Although this invariant should hold both
-- before and after the Create CLI command.
invariantCloudyInstCorectDates :: Connection -> IO [DbInvariantErr]
invariantCloudyInstCorectDates conn = do
  instIds <-
    fmap fromOnly <$>
      query_
        conn
        "SELECT id FROM cloudy_instance WHERE created_at is NULL"
  pure $ fmap CloudyInstanceHasNullCreatedAt instIds
