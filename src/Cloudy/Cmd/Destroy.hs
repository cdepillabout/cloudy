{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Destroy where

import Cloudy.Cli (DestroyCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts)
import Cloudy.Db (CloudyInstanceId, withCloudyDb, findOnlyOneInstanceId, OnlyOne (..), findCloudyInstanceIdByName)
import Data.Text (Text, unpack)
import Database.SQLite.Simple (Connection)

data DestroyBy = DestroyByName Text | DestroyByInstanceId CloudyInstanceId | DestroyOnlyOne
  deriving stock Show

data DestroySettings = DestroySettings
  { destroyBy :: DestroyBy
  }
  deriving stock Show

mkSettings :: LocalConfFileOpts -> DestroyCliOpts -> IO DestroySettings
mkSettings _localConfFileOpts cliOpts = do
  let destroyBy =
        case (cliOpts.id, cliOpts.name) of
          (Just cloudyInstanceId, Nothing) -> DestroyByInstanceId cloudyInstanceId
          (Nothing, Just cloudyInstanceName) -> DestroyByName cloudyInstanceName
          (Nothing, Nothing) -> DestroyOnlyOne
          (_, _) -> error "Both cloudy instance id and cloudy instance name were specified.  You can only specify at most one of these."
  pure DestroySettings { destroyBy }

runDestroy :: LocalConfFileOpts -> DestroyCliOpts -> IO ()
runDestroy localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  print settings
  withCloudyDb $ \conn -> do
    instanceInfo <- findInstanceInfoForDestroyBy conn settings.destroyBy
    undefined

    -- (cloudyInstanceId, instanceName) <- newCloudyInstance conn
    -- currentTime <- getCurrentTime
    -- (scalewayServerId, scalewayIpId, scalewayIpAddr) <- runScalewayClientM
    --   (\err -> error $ "ERROR! Problem creating instance: " <> show err)
    --   (createScalewayServer settings instanceName)
    -- newScalewayInstance
    --   conn
    --   currentTime
    --   cloudyInstanceId
    --   (unServerId scalewayServerId)
    --   (unIpId scalewayIpId)
    --   scalewayIpAddr
    -- putStrLn "Waiting for Scaleway instance to become available..."
    -- runScalewayClientM
    --   (\err -> error $ "ERROR! Problem waiting for instance to be ready: " <> show err)
    --   (waitForScalewayServer settings scalewayServerId)
    -- putStrLn "Scaleway instance now available."

findInstanceInfoForDestroyBy :: Connection -> DestroyBy -> IO a0
findInstanceInfoForDestroyBy conn destroyBy = do
  cloudyInstanceId <- cloudyInstanceIdForDestroyBy conn destroyBy
  instanceInfoForId cloudyInstanceId

cloudyInstanceIdForDestroyBy :: Connection -> DestroyBy -> IO CloudyInstanceId
cloudyInstanceIdForDestroyBy conn = \case
  DestroyByName instName -> do
    maybeCloudyInstId <- findCloudyInstanceIdByName conn instName
    case maybeCloudyInstId of
      Nothing ->
        error . unpack $
          "No cloudy instances found with name \"" <> instName <> "\""
      Just cloudyInstId -> pure cloudyInstId
  DestroyByInstanceId cloudyInstanceId -> pure cloudyInstanceId
  DestroyOnlyOne -> do
    onlyOneInstId <- findOnlyOneInstanceId conn
    case onlyOneInstId of
      OnlyOne instId -> pure instId
      MultipleExist ->
        error
          "Multiple cloudy instances exist in the database, so you must pass \
          \--id or --name to operate on a specific instance."
      NoneExist ->
        error "No cloudy instances exist in the database"

