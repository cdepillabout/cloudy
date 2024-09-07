
module Cloudy.Cmd.Utils where

import Cloudy.Db (CloudyInstanceId, OnlyOne (..), InstanceInfo, findCloudyInstanceIdByName, findOnlyOneInstanceId, instanceInfoForId)
import Data.Text (Text, unpack)
import Database.SQLite.Simple (Connection)

data SelectInstBy = SelectInstByName Text | SelectInstById CloudyInstanceId | SelectInstOnlyOne
  deriving stock Show

mkSelectInstBy :: Maybe CloudyInstanceId -> Maybe Text -> IO SelectInstBy
mkSelectInstBy maybeCloudyInstId maybeCloudyInstName =
  case (maybeCloudyInstId, maybeCloudyInstName) of
    (Just cloudyInstanceId, Nothing) -> pure $ SelectInstById cloudyInstanceId
    (Nothing, Just cloudyInstanceName) -> pure $ SelectInstByName cloudyInstanceName
    (Nothing, Nothing) -> pure SelectInstOnlyOne
    (_, _) -> error "Both cloudy instance id and cloudy instance name were specified.  You can only specify at most one of these."

findCloudyInstanceIdForSelectInstBy :: Connection -> SelectInstBy -> IO CloudyInstanceId
findCloudyInstanceIdForSelectInstBy conn = \case
  SelectInstByName instName -> do
    maybeCloudyInstId <- findCloudyInstanceIdByName conn instName
    case maybeCloudyInstId of
      Nothing ->
        error . unpack $
          "No cloudy instances found with name \"" <> instName <> "\""
      Just cloudyInstId -> pure cloudyInstId
  SelectInstById cloudyInstanceId -> pure cloudyInstanceId
  SelectInstOnlyOne -> do
    onlyOneInstId <- findOnlyOneInstanceId conn
    case onlyOneInstId of
      OnlyOne instId -> pure instId
      MultipleExist ->
        error
          "Multiple cloudy instances exist in the database, so you must pass \
          \--id or --name to operate on a specific instance."
      NoneExist ->
        error "No cloudy instances exist in the database"

findInstanceInfoForSelectInstBy :: Connection -> SelectInstBy -> IO InstanceInfo
findInstanceInfoForSelectInstBy conn selectInstBy = do
  cloudyInstanceId <- findCloudyInstanceIdForSelectInstBy conn selectInstBy
  maybeInstInfo <- instanceInfoForId conn cloudyInstanceId
  case maybeInstInfo of
    Nothing -> error $ "Couldn't find instance info for selection: " <> show selectInstBy
    Just instInfo -> pure instInfo

