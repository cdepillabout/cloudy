{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.List where

import Cloudy.Cli (ListCliOpts (..))
import Cloudy.Db (CloudyInstance (..), InstanceInfo (..), ScalewayInstance (..), cloudyInstanceFromInstanceInfo, findAllInstanceInfos, withCloudyDb, unCloudyInstanceId)
import Cloudy.InstanceSetup.Types (InstanceSetup (..))
import Cloudy.LocalConfFile (LocalConfFileOpts)
import Cloudy.Table (Table (..), printTable, Align (..))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, UTCTime, utcToZonedTime, TimeZone, getCurrentTimeZone)
import Data.Void (absurd)

runList :: LocalConfFileOpts -> ListCliOpts -> IO ()
runList _localConfFileOpts _opts = do
  instInfos <- withCloudyDb $ \conn -> findAllInstanceInfos conn
  displayInstanceInfos instInfos

displayInstanceInfos :: [InstanceInfo] -> IO ()
displayInstanceInfos instInfos = do
  tz <- getCurrentTimeZone
  let sortByCloudyInstId = sortOn (\instInfo -> (cloudyInstanceFromInstanceInfo instInfo).id) instInfos
  case sortByCloudyInstId of
    [] -> putStrLn "No instances currently running."
    (hInst : tInsts) -> do
      let instTable = mkTable tz (hInst :| tInsts)
      printTable instTable

mkTable :: TimeZone -> NonEmpty InstanceInfo -> Table
mkTable tz instanceTypes =
  Table
    { tableHeaders =
        (RightJustified, "instance id") :|
        [ (LeftJustified, "instance name")
        , (LeftJustified, "created date")
        , (LeftJustified, "cloud")
        , (LeftJustified, "zone")
        , (LeftJustified, "ip")
        , (LeftJustified, "instance setup")
        ]
    , tableBodyRows = fmap (mkRow tz) instanceTypes
    }

mkRow :: TimeZone -> InstanceInfo -> NonEmpty Text
mkRow tz = \case
  CloudyAwsInstance _ void -> absurd void
  CloudyScalewayInstance cloudyInstance scalewayInstance ->
    pack (show (unCloudyInstanceId cloudyInstance.id)) :|
    [ cloudyInstance.name
    , formatDateTime cloudyInstance.createdAt
    , "scaleway"
    , scalewayInstance.scalewayZone
    , scalewayInstance.scalewayIpAddress
    , prettyInstanceSetup cloudyInstance.instanceSetup
    ]
  where
    formatDateTime :: Maybe UTCTime -> Text
    formatDateTime = \case
      Nothing -> "ERROR: expecting cloudy instance to have created_at value"
      Just createdAt ->
        pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" $ utcToZonedTime tz createdAt

    prettyInstanceSetup :: Maybe InstanceSetup -> Text
    prettyInstanceSetup = \case
      Nothing -> "(none)"
      Just instSetup -> instSetup.name
