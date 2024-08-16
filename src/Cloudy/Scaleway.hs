{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE EmptyDataDecls #-}

module Cloudy.Scaleway where

import Data.Aeson (ToJSON(..), object, (.=), FromJSON (..), withObject, Value, (.:), withText)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API ((:>), Capture, ReqBody, JSON, AuthProtect, (:<|>) ((:<|>)), PostCreated, Get, QueryParam, Headers, Header, PlainText, NoContent, MimeRender (mimeRender), PatchNoContent, Accept (contentType))
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

newtype UserDataKey = UserDataKey { unUserDataKey :: Text }
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)

newtype UserData = UserData { unUserData :: Text }
  deriving stock (Eq, Show)
  -- deriving newtype (FromHttpApiData, FromJSON, ToHttpApiData, ToJSON)
  deriving newtype (MimeRender PlainTextNoUTF8)

data Volume = Volume
  { name :: Text
  , size :: Int
  , volumeType :: Text
  }
  deriving stock Show

instance ToJSON Volume where
  toJSON volume =
    object
      [ "name" .= volume.name
      , "size" .= volume.size
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
    innerObj <- o .: "server"
    id_ <- innerObj .: "id"
    name <- innerObj .: "name"
    pure ServersResp { id = id_, name }



newtype ProductServersResp = ProductServersResp { unProductServersResp :: Map Text ProductServer }
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
    networkObj <- o .: "network"
    sumInternetBandwidth <- networkObj .: "sum_internal_bandwidth"
    pure ProductServer { monthlyPrice, ncpus, ram, sumInternetBandwidth, arch, altNames }

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

type InstanceServersPostApi =
  AuthProtect "auth-token" :>
  "instance" :>
  "v1" :>
  "zones" :>
  Capture "zone" Zone :>
  "servers" :>
  ReqBody '[JSON] ServersReq :>
  PostCreated '[JSON] ServersResp

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
  InstanceServersPostApi :<|>
  InstanceServersUserDataPatchApi :<|>
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

serversPostApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  ServersReq ->
  ClientM ServersResp

serversUserDataPatchApi ::
  AuthenticatedRequest (AuthProtect "auth-token") ->
  Zone ->
  ServerId ->
  UserDataKey ->
  UserData ->
  ClientM NoContent

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
  :<|> serversPostApi
  :<|> serversUserDataPatchApi
  :<|> productsServersGetApi
  :<|> productsServersAvailabilityGetApi
  :<|> imagesGetApi = client scalewayApi
