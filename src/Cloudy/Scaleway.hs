
module Cloudy.Scaleway where

import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API ((:>), Capture, ReqBody, JSON, Post, AuthProtect, (:<|>) ((:<|>)))
import Servant.Client (client, ClientM)
import Servant.Client.Core (AuthenticatedRequest)
import Web.HttpApiData (ToHttpApiData (..))


-- curl 'https://api.scaleway.com/instance/v1/zones/nl-ams-1/servers'\
--   -X POST\
--     -H 'Accept: application/json'\
--       -H 'Content-Type: application/json; charset=utf-8'\
--         -H 'x-session-token: ey...'\
--           --data-raw '...'

-- type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
--       :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
--             :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Zone = NL1 | NL2 | NL3
  deriving (Eq, Show)

zoneToText :: Zone -> Text
zoneToText = \case
  NL1 -> "nl-ams-1"
  NL2 -> "nl-ams-2"
  NL3 -> "nl-ams-3"

instance ToHttpApiData Zone where
  toUrlPiece :: Zone -> Text
  toUrlPiece = zoneToText

newtype ImageId = ImageId { unImageId :: Text }
newtype IpId = IpId { unIpId :: Text }
newtype OrganizationId = OrganizationId { unOrganizationId :: Text }
newtype ProjectId = ProjectId { unProjectId :: Text }
newtype ServerId = ServerId { unServerId :: Text }

data Volume = Volume
  { size :: Int
  , volumeType :: Text
  }

data IpsReq = IpsReq
  { type_ :: Text
  , project :: ProjectId
  }

data IpsResp = IpsResp
  { id :: IpId
  , address :: Text
  , organization :: OrganizationId
  , project :: ProjectId
  , zone :: Zone
  }

data ServersReq = ServersReq
  { bootType :: Text
  , commercialType :: Text
  , image :: ImageId
  , name :: Text
  , publicIps :: [IpId]
  , tags :: [Text]
  , volumes :: Map Text Volume
  , project :: ProjectId
  }

data ServersResp = ServersResp
  { id :: ServerId
  , name :: Text
  }

type InstanceIpsPostApi = AuthProtect "auth-token" :> "instance" :> "v1" :> "zones" :> Capture "zone" Zone :> "ips" :> ReqBody '[JSON] IpsReq :> Post '[JSON] IpsResp
type InstanceServersPostApi = AuthProtect "auth-token" :> "instance" :> "v1" :> "zones" :> Capture "zone" Zone :> "servers" :> ReqBody '[JSON] ServersReq :> Post '[JSON] ServersResp

type ScalewayApi = InstanceIpsPostApi :<|> InstanceServersPostApi

scalewayApi :: Proxy ScalewayApi
scalewayApi = Proxy

ipsPostApi :: AuthenticatedRequest (AuthProtect "auth-token") -> Zone -> IpsReq -> ClientM IpsResp
serverPostApi :: AuthenticatedRequest (AuthProtect "auth-token") -> Zone -> ServersReq -> ClientM ServersResp
ipsPostApi :<|> serverPostApi = client scalewayApi
