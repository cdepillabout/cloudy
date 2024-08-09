{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Scaleway where

import Data.Aeson (ToJSON(..), object, (.=), FromJSON (..), withObject, Value, (.:), withText)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API ((:>), Capture, ReqBody, JSON, Post, AuthProtect, (:<|>) ((:<|>)))
import Servant.Client (client, ClientM)
import Servant.Client.Core (AuthenticatedRequest)
import Web.HttpApiData (ToHttpApiData (..), FromHttpApiData)
import Data.Aeson.Types (Parser)


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

instance ToJSON Zone where toJSON = toJSON . zoneToText
instance FromJSON Zone where
  parseJSON = withText "Zone" $ maybe (fail "Failed to parse Zone") pure . zoneFromText

zoneToText :: Zone -> Text
zoneToText = \case
  NL1 -> "nl-ams-1"
  NL2 -> "nl-ams-2"
  NL3 -> "nl-ams-3"

zoneFromText :: Text -> Maybe Zone
zoneFromText = \case
  "nl-ams-1" -> Just NL1
  "nl-ams-2" -> Just NL2
  "nl-ams-3" -> Just NL3
  _ -> Nothing


instance ToHttpApiData Zone where
  toUrlPiece :: Zone -> Text
  toUrlPiece = zoneToText

newtype ImageId = ImageId { unImageId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype IpId = IpId { unIpId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype OrganizationId = OrganizationId { unOrganizationId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype ProjectId = ProjectId { unProjectId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype ServerId = ServerId { unServerId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

data Volume = Volume
  { size :: Int
  , volumeType :: Text
  }
  deriving stock Show

instance ToJSON Volume where
  toJSON volume =
    object
      [ "size" .= volume.size
      , "volume_type" .= volume.volumeType
      ]


data IpsReq = IpsReq
  { type_ :: Text
  , project :: ProjectId
  }
  deriving stock Show

instance ToJSON IpsReq where
  toJSON ipsReq =
    object
      [ "type" .= ipsReq.type_
      , "project" .= ipsReq.project
      ]

data IpsResp = IpsResp
  { id :: IpId
  , address :: Text
  , organization :: OrganizationId
  , project :: ProjectId
  , zone :: Zone
  }
  deriving stock Show

instance FromJSON IpsResp where
  parseJSON :: Value -> Parser IpsResp
  parseJSON = withObject "IpsResp outer wrapper" $ \o -> do
    innerObj <- o .: "ips"
    id_ <- innerObj .: "id"
    address <- innerObj .: "address"
    organization <- innerObj .: "organization"
    project <- innerObj .: "project"
    zone <- innerObj .: "zone"
    pure IpsResp { id = id_, address, organization, project, zone }

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
  deriving stock Show

instance ToJSON ServersReq where
  toJSON serversReq =
    object
      [ "boot_type" .= serversReq.bootType
      , "commercial_type" .= serversReq.commercialType
      , "image" .= serversReq.image
      , "name" .= serversReq.name
      , "public_ips" .= serversReq.publicIps
      , "tags" .= serversReq.tags
      , "volumes" .= serversReq.volumes
      , "project" .= serversReq.project
      ]

data ServersResp = ServersResp
  { id :: ServerId
  , name :: Text
  }
  deriving stock Show

instance FromJSON ServersResp where
  parseJSON :: Value -> Parser ServersResp
  parseJSON = withObject "ServersResp outer wrapper" $ \o -> do
    innerObj <- o .: "servers"
    id_ <- innerObj .: "id"
    name <- innerObj .: "name"
    pure ServersResp { id = id_, name }

type InstanceIpsPostApi =
  AuthProtect "auth-token" :> "instance" :> "v1" :> "zones" :> Capture "zone" Zone :> "ips" :> ReqBody '[JSON] IpsReq :> Post '[JSON] IpsResp

type InstanceServersPostApi =
  AuthProtect "auth-token" :> "instance" :> "v1" :> "zones" :> Capture "zone" Zone :> "servers" :> ReqBody '[JSON] ServersReq :> Post '[JSON] ServersResp

type ScalewayApi = InstanceIpsPostApi :<|> InstanceServersPostApi

scalewayApi :: Proxy ScalewayApi
scalewayApi = Proxy

ipsPostApi :: AuthenticatedRequest (AuthProtect "auth-token") -> Zone -> IpsReq -> ClientM IpsResp
serverPostApi :: AuthenticatedRequest (AuthProtect "auth-token") -> Zone -> ServersReq -> ClientM ServersResp
ipsPostApi :<|> serverPostApi = client scalewayApi
