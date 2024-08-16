
module Cloudy.LocalConfFile where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:?), Value (Null), Object)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Yaml (decodeEither')
import System.IO.Error (tryIOError, isDoesNotExistError)
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig), createDirectoryIfMissing)
import System.FilePath ((</>))

data LocalConfFileScalewayOpts = LocalConfFileScalewayOpts
  { accessKey :: Maybe Text
  , secretKey :: Maybe Text
  , defaultOrganizationId :: Maybe Text
  , defaultProjectId :: Maybe Text
  , defaultZone :: Maybe Text
  , defaultInstanceType :: Maybe Text
  , defaultImageId :: Maybe Text
  } deriving Show

instance FromJSON LocalConfFileScalewayOpts where
  parseJSON :: Value -> Parser LocalConfFileScalewayOpts
  parseJSON = withObject "LocalConfFileScalewayOpts" $ \o -> do
    accessKey <- o .:? "access_key"
    secretKey <- o .:? "secret_key"
    defaultOrganizationId <- o .:? "default_organization_id"
    defaultProjectId <- o .:? "default_project_id"
    defaultZone <- o .:? "default_zone"
    defaultInstanceType <- o .:? "default_instance_type"
    defaultImageId <- o .:? "default_image_id"
    pure
      LocalConfFileScalewayOpts
        { accessKey
        , secretKey
        , defaultOrganizationId
        , defaultProjectId
        , defaultZone
        , defaultInstanceType
        , defaultImageId
        }

data LocalConfFileOpts = LocalConfFileOpts
  { scaleway :: Maybe LocalConfFileScalewayOpts
  } deriving Show

instance FromJSON LocalConfFileOpts where
  parseJSON Null = pure defaultLocalConfFileOpts
  parseJSON v = withObject "LocalConfFileOpts" parseObj v
    where
      parseObj :: Object -> Parser LocalConfFileOpts
      parseObj o =  do
        LocalConfFileOpts <$> o .:? "scaleway"

defaultLocalConfFileOpts :: LocalConfFileOpts
defaultLocalConfFileOpts = LocalConfFileOpts { scaleway = Nothing }

getCloudyLocalConfFilePath :: IO FilePath
getCloudyLocalConfFilePath = do
  cloudyConfDirLocal <- getXdgDirectory XdgConfig "cloudy"
  createDirectoryIfMissing True cloudyConfDirLocal
  pure $ cloudyConfDirLocal </> "cloudy.yaml"

readLocalConfFile :: IO LocalConfFileOpts
readLocalConfFile = do
  confFilePath <- getCloudyLocalConfFilePath
  eitherConfFileRaw <- tryIOError (BS.readFile confFilePath)
  case eitherConfFileRaw of
    Left ioEx
      | isDoesNotExistError ioEx ->
          -- conf file does not exist yet on disk, just return the default options
          pure defaultLocalConfFileOpts
      | otherwise ->
          -- We got an IOException that we weren't expecting.  Rethrow it.
          throwIO ioEx
    Right confFileRaw -> do
      let eitherConfFileOpts = decodeEither' confFileRaw
      case eitherConfFileOpts of
        Left parseErr ->
          -- We got an error when trying to parse the config file.  This is unexpected. Throw it.
          throwIO parseErr
        Right confFile -> pure confFile
