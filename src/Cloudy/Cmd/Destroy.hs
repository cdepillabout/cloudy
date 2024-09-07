{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Destroy where

import Cloudy.Cli (DestroyCliOpts (..))
import Cloudy.Cmd.Utils (SelectInstBy, findInstanceInfoForSelectInstBy, mkSelectInstBy)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Db (CloudyInstance (..), withCloudyDb, InstanceInfo (..), ScalewayInstance (..), setCloudyInstanceDeleted, cloudyInstanceFromInstanceInfo)
import Data.Text (Text, unpack)
import Data.Void (absurd)
import Servant.Client (ClientM)
import Servant.API (NoContent(..))
import Cloudy.Cmd.Scaleway.Utils (runScalewayClientM, createAuthReq)
import Cloudy.Scaleway (ipsDeleteApi, zoneFromText, ServersActionReq (..), serversActionPostApi)
import qualified Cloudy.Scaleway as Scaleway
import Control.FromSum (fromMaybeM)
import Control.Monad.IO.Class (liftIO)

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
  selectInstBy <- mkSelectInstBy cliOpts.id cliOpts.name
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
  withCloudyDb $ \conn -> do
    instanceInfo <- findInstanceInfoForSelectInstBy conn settings.selectInstBy
    case instanceInfo of
      CloudyAwsInstance _cloudyInstance void -> absurd void
      CloudyScalewayInstance _cloudyInstance scalewayInstance -> do
        scalewaySettings <- mkScalewaySettings localConfFileOpts
        runScalewayClientM
          (\err -> error $ "ERROR! Problem deleting instance: " <> show err)
          (destroyScalewayServer settings scalewaySettings scalewayInstance)
    let cloudyInstanceId = (cloudyInstanceFromInstanceInfo instanceInfo).id
    setCloudyInstanceDeleted conn cloudyInstanceId

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
