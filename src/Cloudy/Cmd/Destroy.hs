{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Destroy where

import Cloudy.Cli (DestroyCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Db (CloudyInstance (..), CloudyInstanceId, withCloudyDb, findOnlyOneInstanceId, OnlyOne (..), findCloudyInstanceIdByName, InstanceInfo (..), instanceInfoForId, ScalewayInstance (..), setCloudyInstanceDeleted, cloudyInstanceFromInstanceInfo)
import Data.Text (Text, unpack)
import Database.SQLite.Simple (Connection)
import Data.Void (absurd)
import Servant.Client (ClientM)
import Servant.API (NoContent(..))
import Cloudy.Cmd.Scaleway.Utils (runScalewayClientM, createAuthReq)
import Cloudy.Scaleway (ipsDeleteApi, zoneFromText, ServersActionReq (..), serversActionPostApi)
import qualified Cloudy.Scaleway as Scaleway
import Control.FromSum (fromMaybeM)
import Control.Monad.IO.Class (liftIO)

data SelectInstBy = SelectInstByName Text | SelectInstById CloudyInstanceId | SelectInstOnlyOne
  deriving stock Show

data DestroySettings = DestroySettings
  { selectInstBy :: SelectInstBy
  }
  deriving stock Show

data ScalewayDestroySettings = ScalewayDestroySettings
  { secretKey :: Text
  }
  deriving stock Show

mkSettings :: LocalConfFileOpts -> DestroyCliOpts -> IO DestroySettings
mkSettings _localConfFileOpts cliOpts = do
  let selectInstBy =
        case (cliOpts.id, cliOpts.name) of
          (Just cloudyInstanceId, Nothing) -> SelectInstById cloudyInstanceId
          (Nothing, Just cloudyInstanceName) -> SelectInstByName cloudyInstanceName
          (Nothing, Nothing) -> SelectInstOnlyOne
          (_, _) -> error "Both cloudy instance id and cloudy instance name were specified.  You can only specify at most one of these."
  pure DestroySettings { selectInstBy }

mkScalewaySettings :: LocalConfFileOpts -> IO ScalewayDestroySettings
mkScalewaySettings localConfFileOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  pure ScalewayDestroySettings { secretKey }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runDestroy :: LocalConfFileOpts -> DestroyCliOpts -> IO ()
runDestroy localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  print settings
  withCloudyDb $ \conn -> do
    maybeInstanceInfo <- findInstanceInfoForSelectInstBy conn settings.selectInstBy
    instanceInfo <-
      maybe
        (error $ "Couldn't find instance info for selection: " <> show settings.selectInstBy)
        pure
        maybeInstanceInfo
    case instanceInfo of
      CloudyAwsInstance _cloudyInstance void -> absurd void
      CloudyScalewayInstance _cloudyInstance scalewayInstance -> do
        scalewaySettings <- mkScalewaySettings localConfFileOpts
        runScalewayClientM
          (\err -> error $ "ERROR! Problem deleting instance: " <> show err)
          (destroyScalewayServer settings scalewaySettings scalewayInstance)
    let cloudyInstanceId = (cloudyInstanceFromInstanceInfo instanceInfo).id
    setCloudyInstanceDeleted conn cloudyInstanceId

findInstanceInfoForSelectInstBy :: Connection -> SelectInstBy -> IO (Maybe InstanceInfo)
findInstanceInfoForSelectInstBy conn selectInstBy = do
  cloudyInstanceId <- cloudyInstanceIdForSelectInstBy conn selectInstBy
  instanceInfoForId conn cloudyInstanceId

cloudyInstanceIdForSelectInstBy :: Connection -> SelectInstBy -> IO CloudyInstanceId
cloudyInstanceIdForSelectInstBy conn = \case
  SelectInstByName instName -> do
    maybeCloudyInstId <- findCloudyInstanceIdByName conn instName
    case maybeCloudyInstId of
      Nothing ->
        error . unpack $
          "No cloudy instances found with name \"" <> instName <> "\""
      Just cloudyInstId -> pure cloudyInstId
  SelectInstById cloudyInstanceId -> pure cloudyInstanceId
  SelectInstOnlyOne -> do
    onlyOneInstId <- findOnlyOneInstanceId conn
    case onlyOneInstId of
      OnlyOne instId -> pure instId
      MultipleExist ->
        error
          "Multiple cloudy instances exist in the database, so you must pass \
          \--id or --name to operate on a specific instance."
      NoneExist ->
        error "No cloudy instances exist in the database"

destroyScalewayServer ::
  DestroySettings ->
  ScalewayDestroySettings ->
  ScalewayInstance ->
  ClientM ()
destroyScalewayServer _settings scalewaySettings scalewayInstance = do
  let authReq = createAuthReq scalewaySettings.secretKey
      ipId = Scaleway.IpId scalewayInstance.scalewayIpId
      zoneErrMsg =
        "destroyScalewayServer: Could not figure out Scaleway zone from string: " <>
        scalewayInstance.scalewayZone
  zone <-
    fromMaybeM
      (error $ unpack zoneErrMsg)
      (zoneFromText scalewayInstance.scalewayZone)
  NoContent <- ipsDeleteApi authReq zone ipId
  liftIO . putStrLn . unpack $ "Successfully deleted Scaleway IP: " <> scalewayInstance.scalewayIpAddress
  let act = ServersActionReq { action = "terminate" }
      scalewayInstId = Scaleway.ServerId scalewayInstance.scalewayInstanceId
  _task <- serversActionPostApi authReq zone scalewayInstId act
  liftIO . putStrLn . unpack $ "Successfully deleted Scaleway server: " <> scalewayInstance.scalewayInstanceId
