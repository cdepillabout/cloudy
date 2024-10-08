{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE EmptyDataDecls #-}

module Cloudy.Scaleway where

import Data.Aeson (ToJSON(..), object, (.=), FromJSON (..), withObject, Value, (.:), withText)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API ((:>), Capture, ReqBody, JSON, AuthProtect, (:<|>) ((:<|>)), PostCreated, Get, QueryParam, Headers, Header, PlainText, NoContent, MimeRender (mimeRender), PatchNoContent, Accept (contentType), PostAccepted, Patch, DeleteNoContent, MimeUnrender (..))
import Servant.Client (client, ClientM)
import Servant.Client.Core (AuthenticatedRequest)
import Web.HttpApiData (ToHttpApiData (..), FromHttpApiData)
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Data.Time (UTCTime)
import Network.HTTP.Media ((//))

data PlainTextNoUTF8

instance Accept PlainTextNoUTF8 where
  contentType _ = "text" // "plain"

instance MimeRender PlainTextNoUTF8 Text where
  mimeRender _ = mimeRender (Proxy @PlainText)

instance MimeUnrender PlainTextNoUTF8 Text where
  mimeUnrender _ = mimeUnrender (Proxy @PlainText)

newtype PerPage = PerPage { unPerPage :: Int }
  deriving stock Show
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype PageNum = PageNum { unPageNum :: Int }
  deriving stock Show
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

type Paged :: forall k l. (k -> l -> Type) -> k -> l -> Type
type family Paged verb ct resp where
  Paged verb ct resp =
    QueryParam "per_page" PerPage :>
    QueryParam "page" PageNum :>
    verb ct (Headers '[Header "x-total-count" Int] resp)

data Zone = NL1 | NL2 | NL3
  deriving (Bounded, Enum, Eq, Show)


instance ToJSON Zone where toJSON = toJSON . zoneToText
instance FromJSON Zone where
  parseJSON = withText "Zone" $ maybe (fail "Failed to parse Zone") pure . zoneFromText

allScalewayZones :: [Zone]
allScalewayZones = enumFromTo minBound maxBound

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

newtype VolumeId = VolumeId { unVolumeId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype UserDataKey = UserDataKey { unUserDataKey :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype UserData = UserData { unUserData :: Text }
  deriving stock (Eq, Show)
  deriving newtype (MimeRender PlainTextNoUTF8, MimeUnrender PlainTextNoUTF8)

data ServersReqVolume = ServersReqVolume
  { {- name :: Text
  , -} size :: Int
  , volumeType :: Text
  }
  deriving stock Show

instance ToJSON ServersReqVolume where
  toJSON volume =
    object
      [ {- "name" .= volume.name
      , -} "size" .= volume.size
      , "volume_type" .= volume.volumeType
      ]

data ServersRespVolume = ServersRespVolume
  { id :: VolumeId
  , name :: Text
  , size :: Int
  , volumeType :: Text
  }
  deriving stock Show

instance FromJSON ServersRespVolume where
  parseJSON :: Value -> Parser ServersRespVolume
  parseJSON = withObject "ServersRespVolume" $ \o -> do
    id_ <- o .: "id"
    name <- o .: "name"
    size <- o .: "size"
    volumeType <- o .: "volume_type"
    pure ServersRespVolume { id = id_, name, size, volumeType }

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
    innerObj <- o .: "ip"
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
  , volumes :: Map Text ServersReqVolume
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
  , volumes :: Map Text ServersRespVolume
  , state :: Text
  }
  deriving stock Show

instance FromJSON ServersResp where
  parseJSON :: Value -> Parser ServersResp
  parseJSON = withObject "ServersResp outer wrapper" $ \o -> do
    innerObj <- o .: "server"
    id_ <- innerObj .: "id"
    name <- innerObj .: "name"
    volumes <- innerObj .: "volumes"
    state <- innerObj .: "state"
    pure ServersResp { id = id_, name, volumes, state }

data ServersActionReq = ServersActionReq
  { action :: Text
  }
  deriving stock Show

instance ToJSON ServersActionReq where
  toJSON serversActionReq =
    object
      [ "action" .= serversActionReq.action
      ]

data VolumesReq = VolumesReq
  { name :: Text
  }
  deriving stock Show

instance ToJSON VolumesReq where
  toJSON volumesReq =
    object
      [ "name" .= volumesReq.name
      ]

data TaskResp = TaskResp
  { id :: Text
  , description :: Text
  , status :: Text
  }
  deriving stock Show

instance FromJSON TaskResp where
  parseJSON :: Value -> Parser TaskResp
  parseJSON = withObject "TaskResp outer wrapper" $ \o -> do
    innerObj <- o .: "task"
    id_ <- innerObj .: "id"
    description <- innerObj .: "description"
    status <- innerObj .: "status"
    pure TaskResp { id = id_, description, status }

data VolumeConstraint = VolumeConstraint
  { minSize :: Int
  , maxSize :: Int
  }
  deriving stock Show

instance FromJSON VolumeConstraint where
  parseJSON :: Value -> Parser VolumeConstraint
  parseJSON = withObject "VolumeConstraint" $ \o -> do
    minSize <- o .: "min_size"
    maxSize <- o .: "max_size"
    pure VolumeConstraint { minSize, maxSize }

newtype ProductServersResp = ProductServersResp
  { unProductServersResp :: Map Text ProductServer }
  deriving stock Show

instance FromJSON ProductServersResp where
  parseJSON :: Value -> Parser ProductServersResp
  parseJSON = withObject "ProductServersResp outer wrapper" $ \o -> do
    productServers <- o .: "servers"
    pure $ ProductServersResp productServers

data ProductServer = ProductServer
  { monthlyPrice :: Float
  , ncpus :: Int
  , ram :: Int
  , arch :: Text
  , sumInternetBandwidth :: Int
  , altNames :: [Text]
  , volumesConstraint :: VolumeConstraint
  }
  deriving stock Show

instance FromJSON ProductServer where
  parseJSON :: Value -> Parser ProductServer
  parseJSON = withObject "ProductServer" $ \o -> do
    altNames <- o .: "alt_names"
    monthlyPrice <- o .: "monthly_price"
    ncpus <- o .: "ncpus"
    ram <- o .: "ram"
    arch <- o .: "arch"
    volumesConstraint <- o .: "volumes_constraint"
    networkObj <- o .: "network"
    sumInternetBandwidth <- networkObj .: "sum_internal_bandwidth"
    pure ProductServer
      { monthlyPrice
      , ncpus
      , ram
      , sumInternetBandwidth
      , arch
      , altNames
      , volumesConstraint
      }

newtype ProductServersAvailabilityResp = ProductServersAvailabilityResp { unProductServersAvailabilityResp :: Map Text Text }
  deriving stock Show

instance FromJSON ProductServersAvailabilityResp where
  parseJSON :: Value -> Parser ProductServersAvailabilityResp
  parseJSON = withObject "ProductServersAvailabilityResp outer wrapper" $ \o -> do
    serversObj :: Map Text Value <- o .: "servers"
    availabilityMap <- traverse parseAvail serversObj
    pure $ ProductServersAvailabilityResp availabilityMap
    where
      parseAvail :: Value -> Parser Text
      parseAvail = withObject "ProductServersAvailability availability obj" $ \a -> a .: "availability"

newtype ImagesResp = ImagesResp { unImagesResp :: [Image] }
  deriving stock Show

instance FromJSON ImagesResp where
  parseJSON :: Value -> Parser ImagesResp
  parseJSON = withObject "ImagesResp outer wrapper" $ \o -> do
    images <- o .: "images"
    pure $ ImagesResp images

data Image = Image
  { id :: Text
  , name :: Text
  , arch :: Text
  , creationDate :: UTCTime
  , modificationDate :: UTCTime
  , state :: Text
  , rootVolId :: Text
  , rootVolName :: Text
  , rootVolType :: Text
  , rootVolSize :: Int
  }
  deriving stock Show

instance FromJSON Image where
  parseJSON :: Value -> Parser Image
  parseJSON = withObject "Image" $ \o -> do
    id' <- o .: "id"
    name <- o .: "name"
    arch <- o .: "arch"
    creationDate <- o .: "creation_date"
    modificationDate <- o .: "modification_date"
    state <- o .: "state"
    rootVol <- o .: "root_volume"
    rootVolId <- rootVol .: "id"
    rootVolName <- rootVol .: "name"
    rootVolType <- rootVol .: "volume_type"
    rootVolSize <- rootVol .: "size"
    pure
      Image
        { id = id'
        , name
        , arch
        , creationDate
        , modificationDate
        , state
        , rootVolId
        , rootVolName
        , rootVolType
        , rootVolSize
        }

type InstanceIpsPostApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "ips" :>
  ReqBody '[JSON] IpsReq :>
  PostCreated '[JSON] IpsResp

type InstanceIpsDeleteApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "ips" :>
  Capture "ip_id" IpId :>
  DeleteNoContent

type InstanceServersPostApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "servers" :>
  ReqBody '[JSON] ServersReq :>
  PostCreated '[JSON] ServersResp

type InstanceServersGetApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "servers" :>
  Capture "server_id" ServerId :>
  Get '[JSON] ServersResp

type InstanceServersActionPostApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "servers" :>
  Capture "server_id" ServerId :>
  "action" :>
  ReqBody '[JSON] ServersActionReq :>
  PostAccepted '[JSON] TaskResp

type InstanceServersUserDataGetApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "servers" :>
  Capture "server_id" ServerId :>
  "user_data" :>
  Capture "key" UserDataKey :>
  Get '[PlainTextNoUTF8] UserData

type InstanceServersUserDataPatchApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "servers" :>
  Capture "server_id" ServerId :>
  "user_data" :>
  Capture "key" UserDataKey :>
  ReqBody '[PlainTextNoUTF8] UserData :>
  PatchNoContent

type InstanceVolumesPatchApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "volumes" :>
  Capture "volume_id" VolumeId :>
  ReqBody '[JSON] VolumesReq :>
  Patch '[JSON] Value

type InstanceProductsServersGetApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "products" :>
  "servers" :>
  QueryParam "per_page" PerPage :>
  Get '[JSON] ProductServersResp

type InstanceProductsServersAvailabilityGetApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "products" :>
  "servers" :>
  "availability" :>
  QueryParam "per_page" PerPage :>
  Get '[JSON] ProductServersAvailabilityResp

type InstanceImagesGetApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "images" :>
  QueryParam "arch" Text :>
  Paged Get '[JSON] ImagesResp

type ScalewayApi =
  InstanceIpsPostApi :<|>
  InstanceIpsDeleteApi :<|>
  InstanceServersPostApi :<|>
  InstanceServersGetApi :<|>
  InstanceServersActionPostApi :<|>
  InstanceServersUserDataGetApi :<|>
  InstanceServersUserDataPatchApi :<|>
  InstanceVolumesPatchApi :<|>
  InstanceProductsServersGetApi :<|>
  InstanceProductsServersAvailabilityGetApi :<|>
  InstanceImagesGetApi

scalewayApi :: Proxy ScalewayApi
scalewayApi = Proxy

ipsPostApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  IpsReq ->
  ClientM IpsResp

ipsDeleteApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  IpId ->
  ClientM NoContent

serversPostApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  ServersReq ->
  ClientM ServersResp

serversGetApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  ServerId ->
  ClientM ServersResp

serversActionPostApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  ServerId ->
  ServersActionReq ->
  ClientM TaskResp

serversUserDataGetApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  ServerId ->
  UserDataKey ->
  ClientM UserData

serversUserDataPatchApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  ServerId ->
  UserDataKey ->
  UserData ->
  ClientM NoContent

volumesPatchApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  VolumeId ->
  VolumesReq ->
  ClientM Value

productsServersGetApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  Maybe PerPage ->
  ClientM ProductServersResp

productsServersAvailabilityGetApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  Maybe PerPage ->
  ClientM ProductServersAvailabilityResp

imagesGetApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  Maybe Text ->
  Maybe PerPage ->
  Maybe PageNum ->
  ClientM (Headers '[Header "x-total-count" Int] ImagesResp)

ipsPostApi
  :<|> ipsDeleteApi
  :<|> serversPostApi
  :<|> serversGetApi
  :<|> serversActionPostApi
  :<|> serversUserDataGetApi
  :<|> serversUserDataPatchApi
  :<|> volumesPatchApi
  :<|> productsServersGetApi
  :<|> productsServersAvailabilityGetApi
  :<|> imagesGetApi = client scalewayApi
