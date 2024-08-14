{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.ListInstanceTypes where

import Cloudy.Cli.Scaleway (ScalewayListInstanceTypesCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, getZone, runScalewayClientM)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Scaleway (Zone (..), productsServersGetApi, ProductServersResp (..), ProductServer (..), ProductServersAvailabilityResp (..), productsServersAvailabilityGetApi, PerPage (PerPage))
import Cloudy.Table (printTable, Table (..), Align (..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Merge.Strict (merge, mapMissing, dropMissing, zipWithMatched)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Servant.Client (ClientM)
import Text.Printf (printf)
import qualified Data.Text as Text

data ScalewayListInstanceTypesSettings = ScalewayListInstanceTypesSettings
  { secretKey :: Text
  , zone :: Zone
  }

mkSettings :: LocalConfFileOpts -> ScalewayListInstanceTypesCliOpts -> IO ScalewayListInstanceTypesSettings
mkSettings localConfFileOpts cliOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeZoneFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultZone
  zone <- getZone maybeZoneFromConfFile cliOpts.zone
  pure ScalewayListInstanceTypesSettings { secretKey, zone }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runListInstanceTypes :: LocalConfFileOpts -> ScalewayListInstanceTypesCliOpts -> IO ()
runListInstanceTypes localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  instanceTypes <-
    runScalewayClientM
      (\err -> error $ "ERROR! Problem fetching instance types: " <> show err)
      (fetchInstanceTypes settings)
  displayInstanceTypes instanceTypes

fetchInstanceTypes :: ScalewayListInstanceTypesSettings -> ClientM (Map Text (ProductServer, Text))
fetchInstanceTypes settings = do
  let authReq = createAuthReq settings.secretKey
      numPerPage = 100
  ProductServersResp productServers <- productsServersGetApi authReq settings.zone (Just $ PerPage numPerPage)
  let numProductServers = length $ Map.elems productServers
  when (numProductServers == numPerPage) $
    liftIO $ putStrLn "WARNING: The number of instance types returned is equal to the max per page.  PROPER PAGING NEEDS TO BE IMPLEMENTED! We are likely missing instance types...."
  ProductServersAvailabilityResp avail <- productsServersAvailabilityGetApi authReq settings.zone (Just $ PerPage numPerPage)
  let numAvail = length $ Map.elems avail
  when (numAvail == numPerPage) $
    liftIO $ putStrLn "WARNING: The number of availabilities returned is equal to the max per page.  PROPER PAGING NEEDS TO BE IMPLEMENTED! We are likely missing instance types...."
  pure $
    merge
      (mapMissing (\_ prod -> (prod, "UNKNOWN")))
      dropMissing
      (zipWithMatched (\_ -> (,)))
      productServers
      avail

displayInstanceTypes :: Map Text (ProductServer, Text) -> IO ()
displayInstanceTypes instanceTypes = do
  let instList = Map.toList instanceTypes
      sortByPriceInstList = sortOn (\(_, (prod, _)) -> prod.monthlyPrice) instList
  case sortByPriceInstList of
    [] -> undefined
    (hInst : tInsts) -> do
      let instTable = mkTable (hInst :| tInsts)
      printTable instTable

mkTable :: NonEmpty (Text, (ProductServer, Text)) -> Table
mkTable instanceTypes =
  Table
    { tableHeaders =
        (LeftJustified, "instance type id") :|
        [ (RightJustified, "monthly cost")
        , (LeftJustified, "architecture")
        , (RightJustified, "cpus")
        , (RightJustified, "memory")
        , (RightJustified, "bandwidth")
        , (LeftJustified, "availability")
        , (LeftJustified, "alt names")
        ]
    , tableBodyRows = fmap mkRow instanceTypes
    }

mkRow :: (Text, (ProductServer, Text)) -> NonEmpty Text
mkRow (instType, (prod, availability)) =
  instType :|
  [ "€" <> pack (printf "% 8.2f" prod.monthlyPrice)
  , prod.arch
  , pack $ show prod.ncpus
  , pack $ printf "% 8.01f gib" (fromIntegral prod.ram / oneGib :: Double)
  , pack $ printf "% 8.03f gbps" (fromIntegral prod.sumInternetBandwidth / oneGb :: Double)
  , availability
  , case prod.altNames of
      [] -> "(none)"
      names -> Text.intercalate ", " names
  ]

oneGib :: Num a => a
oneGib = 1024 * 1024 * 1024

oneGb :: Num a => a
oneGb = 1000 * 1000 * 1000
