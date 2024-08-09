module Cloudy.Cmd.Create where

import Cloudy.Cli (CreateCliOpts (..), AwsCliOpts (..), ScalewayCliOpts (..))
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), ProjectId (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, BaseUrl (BaseUrl), Scheme (Https), runClientM, ClientM)
import Servant.Client.Core (mkAuthenticatedRequest, AuthenticatedRequest, AuthClientData, Request, addHeader)
import Servant.API (AuthProtect)

runCreate :: CreateCliOpts -> IO ()
runCreate = \case
  CreateCliOptsScaleway scalewayOpts -> runCreateScaleway scalewayOpts
  CreateCliOptsAws awsOpts -> runCreateAws awsOpts

runCreateAws :: AwsCliOpts -> IO ()
runCreateAws awsOpts = undefined


runCreateScaleway :: ScalewayCliOpts -> IO ()
runCreateScaleway scalewayOpts = do
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager scalewayBaseUrl
  res <- runClientM go clientEnv
  print res
  where
    go :: ClientM ()
    go = do
      let authData = "testtesttest"
      let authReq = mkAuthenticatedRequest authData blahblah :: AuthenticatedRequest (AuthProtect "auth-token")
      res <- ipsPostApi authReq NL1 (IpsReq "routed_ipv4" $ ProjectId "62265224-89aa-4d04-b129-6b405569fdea")
      liftIO $ putStrLn $ "ips res: " <> show res

blahblah :: Text -> Request -> Request
blahblah authData = addHeader "X-Auth-Token" authData

type instance AuthClientData (AuthProtect "auth-token") = Text

scalewayBaseUrl :: BaseUrl
scalewayBaseUrl = BaseUrl Https "api.scaleway.com" 443 ""
