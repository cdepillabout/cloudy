{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.Create where

import Cloudy.Cli.Scaleway (ScalewayCreateCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, scalewayBaseUrl, getZone)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.NameGen (instanceNameGen)
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), ProjectId (..), serversPostApi, ServersReq (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, runClientM, ClientM)

data ScalewayCreateSettings = ScalewayCreateSettings
  { secretKey :: Text
  , projectId :: ProjectId
  , zone :: Zone
  }

mkSettings :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ScalewayCreateSettings
mkSettings localConfFileOpts cliOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeProjectId = localConfFileOpts.scaleway >>= \scale -> fmap ProjectId scale.defaultProjectId
  projectId <- getVal maybeProjectId "Could not find scaleway.default_project_id in config file"
  let maybeZoneFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultZone
  zone <- getZone maybeZoneFromConfFile cliOpts.zone
  pure ScalewayCreateSettings { secretKey, projectId, zone }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runCreate :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ()
runCreate localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager scalewayBaseUrl
  res <- runClientM (go settings) clientEnv
  print res
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
              , commercialType {- :: Text -} = undefined
              , image {- :: ImageId -} = undefined
              , name {- :: Text -} = serverName
              , publicIps {- :: [IpId] -} = undefined
              , tags {- :: [Text] -} = undefined
              , volumes {- :: Map Text Volume -} = undefined
              , project {- :: ProjectId -} = settings.projectId
              }
      serversResp <- serversPostApi authReq settings.zone serversReq
      liftIO $ putStrLn $ "servers resp: " <> show serversResp
