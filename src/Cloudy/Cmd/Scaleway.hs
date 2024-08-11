{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway where

import Cloudy.Cli (ScalewayCliOpts (..))
import Cloudy.Cli.Scaleway (ScalewayCreateCliOpts (..), ScalewayListInstanceTypesCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.NameGen (instanceNameGen)
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), ProjectId (..), zoneFromText, serversPostApi, ServersReq (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, unpack)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (AuthProtect)
import Servant.Client (mkClientEnv, BaseUrl (BaseUrl), Scheme (Https), runClientM, ClientM)
import Servant.Client.Core (mkAuthenticatedRequest, AuthenticatedRequest, AuthClientData, Request, addHeader)

createAuthReq :: ScalewayCreateSettings -> AuthenticatedRequest (AuthProtect "auth-token")
createAuthReq settings = mkAuthenticatedRequest settings.secretKey createAuthTokenHeader

createAuthTokenHeader :: Text -> Request -> Request
createAuthTokenHeader authData = addHeader "X-Auth-Token" authData

type instance AuthClientData (AuthProtect "auth-token") = Text

scalewayBaseUrl :: BaseUrl
scalewayBaseUrl = BaseUrl Https "api.scaleway.com" 443 ""

runScaleway :: LocalConfFileOpts -> ScalewayCliOpts -> IO ()
runScaleway localConfFileOpts = \case
  ScalewayCreate scalewayCreateCliOpts -> runCreate localConfFileOpts scalewayCreateCliOpts
  ScalewayListInstanceTypes scalewayListInstanceTypesCliOpts -> runListInstanceTypes localConfFileOpts scalewayListInstanceTypesCliOpts

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

getZone :: Maybe Text -> Maybe Text -> IO Zone
getZone maybeZoneFromConfFile maybeZoneFromCliOpts =
  case (maybeZoneFromConfFile, maybeZoneFromCliOpts) of
    (_, Just zoneFromCliOpts) ->
      case zoneFromText zoneFromCliOpts of
        Nothing ->
          error . unpack $
            "Could not parse zone specified in --zone option on cli: " <> zoneFromCliOpts
        Just zone -> pure zone
    (Just zoneFromConfFile, _) ->
      case zoneFromText zoneFromConfFile of
        Nothing ->
          error . unpack $
            "Could not parse zone specified in scaleway.defaultZone in config file: " <> zoneFromConfFile
        Just zone -> pure zone
    (Nothing, Nothing) -> pure NL1

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
      let authReq = createAuthReq settings
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

runListInstanceTypes :: LocalConfFileOpts -> ScalewayListInstanceTypesCliOpts -> IO ()
runListInstanceTypes localConfFileOpts listInstanceTypesCliOpts = undefined
