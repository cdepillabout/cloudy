{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.Create where

import Cloudy.Cli.Scaleway (ScalewayCreateCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, getZone, runScalewayClientM, getInstanceType, getImageId)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Db (newCloudyInstance, newScalewayInstance, withCloudyDb)
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), IpsResp (..), ProjectId (..), serversPostApi, ServersReq (..), ServersResp (..), ImageId (ImageId), serversUserDataPatchApi, UserDataKey (UserDataKey), UserData (UserData), ServersActionReq (..), serversActionPostApi, ServersRespVolume (..), ServersReqVolume (..), VolumesReq (..), volumesPatchApi, ServerId, unServerId)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Servant.Client (ClientM)
import Servant.API (NoContent(NoContent))

data ScalewayCreateSettings = ScalewayCreateSettings
  { secretKey :: Text
  , projectId :: ProjectId
  , zone :: Zone
  , instanceType :: Text
  , volumeSizeGb :: Int
  , imageId :: Text
  }

mkSettings :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ScalewayCreateSettings
mkSettings localConfFileOpts cliOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeProjectId = localConfFileOpts.scaleway >>= \scale -> fmap ProjectId scale.defaultProjectId
  projectId <- getVal maybeProjectId "Could not find scaleway.default_project_id in config file"
  let maybeZoneFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultZone
  zone <- getZone maybeZoneFromConfFile cliOpts.zone
  let maybeInstanceTypeFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultInstanceType
      instanceType = getInstanceType maybeInstanceTypeFromConfFile cliOpts.instanceType
  let maybeImageIdFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultImageId
      imageId = getImageId maybeImageIdFromConfFile cliOpts.imageId
  pure
    ScalewayCreateSettings
      { secretKey
      , projectId
      , zone
      , instanceType
      , volumeSizeGb = cliOpts.volumeSizeGb
      , imageId
      }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runCreate :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ()
runCreate localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  withCloudyDb $ \conn -> do
    (cloudyInstanceId, instanceName) <- newCloudyInstance conn
    currentTime <- getCurrentTime
    scalewayServerId <- runScalewayClientM
      (\err -> error $ "ERROR! Problem creating image: " <> show err)
      (createScalewayServer settings instanceName)
    newScalewayInstance conn currentTime cloudyInstanceId (unServerId scalewayServerId)

createScalewayServer :: ScalewayCreateSettings -> Text -> ClientM ServerId
createScalewayServer settings instanceName = do
  let authReq = createAuthReq settings.secretKey
  ipsRes <- ipsPostApi authReq settings.zone (IpsReq "routed_ipv4" settings.projectId)
  liftIO $ putStrLn $ "ips resp: " <> show ipsRes
  let serversReq =
        ServersReq
          { bootType = "local"
          , commercialType = "cloudy-" <> settings.instanceType
          , image = ImageId settings.imageId
          , name = instanceName
          , publicIps = [ipsRes.id]
          , tags = ["cloudy"]
          , volumes =
              Map.fromList
                [ ( "0"
                  , ServersReqVolume
                      { size = settings.volumeSizeGb * oneGb
                      , volumeType = "b_ssd"
                      }
                  )
                ]
          , project = settings.projectId
          }
  serversResp <- serversPostApi authReq settings.zone serversReq
  liftIO $ putStrLn $ "servers resp: " <> show serversResp
  let maybeFirstVol = Map.lookup "0" serversResp.volumes
  firstVol <- maybe (error "couldn't find first volume, unexpected") pure maybeFirstVol
  -- The volume's name has to initial be created as empty (""), and only
  -- after that can we set the name separately.
  serversVolumesResp <-
    volumesPatchApi
      authReq
      settings.zone
      firstVol.id
      (VolumesReq $ "cloudy-" <> instanceName <> "-boot-block-volume")
  liftIO $ putStrLn $ "servers volumes resp: " <> show serversVolumesResp
  let userData =
        unlines
          [ "#!/usr/bin/env bash"
          , "echo 'hello' >> /whatwhat"
          ]
  NoContent <-
    serversUserDataPatchApi
      authReq
      settings.zone
      serversResp.id
      (UserDataKey "cloud-init")
      (UserData $ pack userData)
  liftIO $ putStrLn "created user data"
  let act = ServersActionReq { action = "poweron" }
  task <- serversActionPostApi authReq settings.zone serversResp.id act
  liftIO $ print task
  pure serversResp.id

oneGb :: Int
oneGb = 1000 * 1000 * 1000
