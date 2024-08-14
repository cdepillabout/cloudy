{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.Create where

import Cloudy.Cli.Scaleway (ScalewayCreateCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, getZone, runScalewayClientM, getInstanceType)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.NameGen (instanceNameGen)
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), IpsResp (..), ProjectId (..), serversPostApi, ServersReq (..), ImageId (ImageId), Volume (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant.Client (ClientM)
import qualified Data.Map.Strict as Map

data ScalewayCreateSettings = ScalewayCreateSettings
  { secretKey :: Text
  , projectId :: ProjectId
  , zone :: Zone
  , instanceType :: Text
  , volumeSizeGb :: Int
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
  pure
    ScalewayCreateSettings
      { secretKey
      , projectId
      , zone
      , instanceType
      , volumeSizeGb = cliOpts.volumeSizeGb
      }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runCreate :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ()
runCreate localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  runScalewayClientM
    (\err -> error $ "ERROR! Problem creating image: " <> show err)
    (go settings)
  where
    go :: ScalewayCreateSettings -> ClientM ()
    go settings = do
      let authReq = createAuthReq settings.secretKey
      ipsRes <- ipsPostApi authReq settings.zone (IpsReq "routed_ipv4" settings.projectId)
      liftIO $ putStrLn $ "ips resp: " <> show ipsRes
      serverName <- liftIO instanceNameGen
      let serversReq =
            ServersReq
              { bootType = "local"
              , commercialType = settings.instanceType
              , image = ImageId "Ubuntu 24.04 Noble Numbat"
              , name = serverName
              , publicIps = [ipsRes.id]
              , tags = ["cloudy"]
              , volumes =
                  Map.fromList
                    [ ( "0"
                      , Volume
                          { name = serverName <> "-boot-block-volume"
                          , size = settings.volumeSizeGb * oneGb
                          , volumeType = "b_ssd"
                          }
                      )
                    ]
              , project = settings.projectId
              }
      serversResp <- serversPostApi authReq settings.zone serversReq
      liftIO $ putStrLn $ "servers resp: " <> show serversResp

oneGb :: Int
oneGb = 1000 * 1000 * 1000
