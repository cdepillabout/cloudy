{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.Create where

import Cloudy.Cli.Scaleway (ScalewayCreateCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, getZone, runScalewayClientM)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.NameGen (instanceNameGen)
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), IpsResp (..), ProjectId (..), serversPostApi, ServersReq (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant.Client (ClientM)

data ScalewayCreateSettings = ScalewayCreateSettings
  { secretKey :: Text
  , projectId :: ProjectId
  , zone :: Zone
  , instanceType :: Text
  }

mkSettings :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ScalewayCreateSettings
mkSettings localConfFileOpts cliOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeProjectId = localConfFileOpts.scaleway >>= \scale -> fmap ProjectId scale.defaultProjectId
  projectId <- getVal maybeProjectId "Could not find scaleway.default_project_id in config file"
  let maybeZoneFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultZone
  zone <- getZone maybeZoneFromConfFile cliOpts.zone
  let maybeInstanceType = localConfFileOpts.scaleway >>= \scale -> scale.defaultInstanceType
  instanceType <- getVal maybeInstanceType "Could not find scaleway.default_instance_type in config file"
  pure ScalewayCreateSettings { secretKey, projectId, zone, instanceType }
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
              { bootType {- :: Text -} = "local"
              , commercialType {- :: Text -} = settings.instanceType
              , image {- :: ImageId -} = undefined
              , name {- :: Text -} = serverName
              , publicIps {- :: [IpId] -} = [ipsRes.id]
              , tags {- :: [Text] -} = ["cloudy"]
              , volumes {- :: Map Text Volume -} = undefined
              , project {- :: ProjectId -} = settings.projectId
              }
      serversResp <- serversPostApi authReq settings.zone serversReq
      liftIO $ putStrLn $ "servers resp: " <> show serversResp
