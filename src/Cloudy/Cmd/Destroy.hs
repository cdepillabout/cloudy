{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Destroy where

import Cloudy.Cli (DestroyCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts)
import Cloudy.Db (CloudyInstanceId, withCloudyDb, findOnlyOneInstanceId, OnlyOne (..), findCloudyInstanceIdByName, InstanceInfo (..), instanceInfoForId, ScalewayInstance (..))
import Data.Text (Text, unpack)
import Database.SQLite.Simple (Connection)
import Data.Void (absurd)
import Data.Maybe (fromMaybe)
import Servant.Client (ClientM)
import Servant.API (NoContent(..))
import Cloudy.Cmd.Scaleway.Utils (runScalewayClientM, createAuthReq)
import Cloudy.Scaleway (ipsDeleteApi)
import qualified Cloudy.Scaleway as Scaleway

data SelectInstBy = SelectInstByName Text | SelectInstById CloudyInstanceId | SelectInstOnlyOne
  deriving stock Show

data DestroySettings = DestroySettings
  { selectInstBy :: SelectInstBy
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
      CloudyScalewayInstance cloudyInstance scalewayInstance -> do
        runScalewayClientM
          (\err -> error $ "ERROR! Problem deleting instance: " <> show err)
          (destroyScalewayServer settings scalewayInstance)

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

destroyScalewayServer :: DestroySettings -> ScalewayInstance -> ClientM ()
destroyScalewayServer settings scalewayInstance = do
  let authReq = createAuthReq settings.secretKey
      ipId = Scaleway.IpId scalewayInstance.scalewayIpId
  zone <-
    fromMaybeM
      _
      (zoneFromText scalewayInstance.scalewayZone)
  NoContent <- ipsDeleteApi authReq ipId zone
  pure ()
