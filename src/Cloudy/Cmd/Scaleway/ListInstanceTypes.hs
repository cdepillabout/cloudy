{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.ListInstanceTypes where

import Cloudy.Cli.Scaleway (ScalewayListInstanceTypesCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, scalewayBaseUrl, getZone)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Scaleway (Zone (..), productsServersPostApi, ProductServersResp (..), ProductServer (..))
import Cloudy.Table (printTable, Table (..), Align (..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn, foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack, pack)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, runClientM, ClientM)
import Text.Pretty.Simple (pPrint)
import Text.Printf (printf)

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
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager scalewayBaseUrl
  res <- runClientM (fetchInstanceTypes settings) clientEnv
  case res of
    Left err -> putStrLn $ "ERROR! Problem fetching instance types: " <> show err
    Right instanceTypes -> displayInstanceTypes instanceTypes

fetchInstanceTypes :: ScalewayListInstanceTypesSettings -> ClientM (Map Text ProductServer)
fetchInstanceTypes settings = do
  let authReq = createAuthReq settings.secretKey
      numPerPage = 100
  ProductServersResp productServers <- productsServersPostApi authReq settings.zone (Just numPerPage)
  let numProductServers = length $ Map.elems productServers
  when (numProductServers == numPerPage) $
    liftIO $ putStrLn "WARNING: The number of instance types returned is equal to the max per page.  PROPER PAGING NEEDS TO BE IMPLEMENTED! We are likely missing instance types...."
  pure productServers

displayInstanceTypes :: Map Text ProductServer -> IO ()
displayInstanceTypes instanceTypes = do
  let instList = Map.toList instanceTypes
      sortByPriceInstList = sortOn (\(_, prod) -> prod.monthlyPrice) instList
  case sortByPriceInstList of
    [] -> undefined
    (hInst : tInsts) -> do
      let instTable = mkTable (hInst :| tInsts)
      printTable instTable

mkTable :: NonEmpty (Text, ProductServer) -> Table
mkTable instanceTypes =
  Table
    { tableHeaders =
        (LeftJustified, "instance type id") :|
        [ (RightJustified, "monthly cost")
        , (LeftJustified, "architecture")
        , (RightJustified, "cpus")
        , (RightJustified, "memory")
        , (RightJustified, "bandwidth")
        ]
    , tableBodyRows = fmap mkRow instanceTypes
    }

mkRow :: (Text, ProductServer) -> NonEmpty Text
mkRow (instType, prod) =
  instType :|
  [ "€ " <> pack (printf "% 8.2f" prod.monthlyPrice)
  , prod.arch
  , pack $ show prod.ncpus
  , pack $ printf "% 8.01f gb" (fromIntegral prod.ram / oneGib :: Double)
  , pack $ show prod.sumInternetBandwidth
  ]

oneGib :: Num a => a
oneGib = 1024 * 1024 * 1024
