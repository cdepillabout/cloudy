{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Create.Scaleway where

import Cloudy.Cli (ScalewayCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), ProjectId (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, BaseUrl (BaseUrl), Scheme (Https), runClientM, ClientM)
import Servant.Client.Core (mkAuthenticatedRequest, AuthenticatedRequest, AuthClientData, Request, addHeader)
import Servant.API (AuthProtect)

data CreateScalewaySettings = CreateScalewaySettings
  { secretKey :: Text
  , projectId :: Text
  }

mkSettings :: LocalConfFileOpts -> ScalewayCliOpts -> IO CreateScalewaySettings
mkSettings localConfFileOpts _ = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeProjectId = localConfFileOpts.scaleway >>= \scale -> scale.defaultProjectId
  projectId <- getVal maybeProjectId "Could not find scaleway.default_project_id in config file"
  pure CreateScalewaySettings { secretKey, projectId }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runCreateScaleway :: LocalConfFileOpts -> ScalewayCliOpts -> IO ()
runCreateScaleway localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager scalewayBaseUrl
  res <- runClientM (go settings) clientEnv
  print res
  where
    go :: CreateScalewaySettings -> ClientM ()
    go settings = do
      let authReq = createAuthReq settings
      res <- ipsPostApi authReq NL1 (IpsReq "routed_ipv4" $ ProjectId settings.projectId)
      liftIO $ putStrLn $ "ips res: " <> show res

createAuthReq :: CreateScalewaySettings -> AuthenticatedRequest (AuthProtect "auth-token")
createAuthReq settings = mkAuthenticatedRequest settings.secretKey createAuthTokenHeader

createAuthTokenHeader :: Text -> Request -> Request
createAuthTokenHeader authData = addHeader "X-Auth-Token" authData

type instance AuthClientData (AuthProtect "auth-token") = Text

scalewayBaseUrl :: BaseUrl
scalewayBaseUrl = BaseUrl Https "api.scaleway.com" 443 ""
