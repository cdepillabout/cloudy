
module Cloudy.Cmd.Scaleway where

import Cloudy.Cli (ScalewayCliOpts (..))
import Cloudy.Cli.Scaleway (ScalewayCreateCliOpts (..), ScalewayListInstanceTypesCliOpts (..))
import Cloudy.Cmd.Scaleway.Create
import Cloudy.Cmd.Scaleway.ListInstanceTypes
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.NameGen (instanceNameGen)
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), ProjectId (..), zoneFromText, serversPostApi, ServersReq (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, unpack)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (AuthProtect)
import Servant.Client (mkClientEnv, BaseUrl (BaseUrl), Scheme (Https), runClientM, ClientM)
import Servant.Client.Core (mkAuthenticatedRequest, AuthenticatedRequest, AuthClientData, Request, addHeader)

runScaleway :: LocalConfFileOpts -> ScalewayCliOpts -> IO ()
runScaleway localConfFileOpts = \case
  ScalewayCreate scalewayCreateCliOpts -> runCreate localConfFileOpts scalewayCreateCliOpts
  ScalewayListInstanceTypes scalewayListInstanceTypesCliOpts -> runListInstanceTypes localConfFileOpts scalewayListInstanceTypesCliOpts
