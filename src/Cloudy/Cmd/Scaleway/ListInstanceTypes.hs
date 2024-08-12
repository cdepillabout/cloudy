{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.ListInstanceTypes where

import Cloudy.Cli.Scaleway (ScalewayListInstanceTypesCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, scalewayBaseUrl, getZone)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Scaleway (Zone (..), productsServersPostApi, ProductServersResp (..), ProductServer (..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack, pack)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, runClientM, ClientM)
import Text.Pretty.Simple (pPrint)
import Data.List (sortOn, foldl')
import Text.Tabular (Table (Table), SemiTable, row, (+.+), col, (^|^), (^..^), colH, Header (..), Properties (..))
import Text.Tabular.AsciiArt (render)
import Text.Printf (printf)
import Data.List.NonEmpty (NonEmpty)
import qualified Text.Tabular as Tabular

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
  -- pPrint sortByPriceInstList
      instTable = mkTable sortByPriceInstList
  putStrLn $ render unpack unpack unpack instTable

mkTable :: [(Text, ProductServer)] -> Table Text Text Text
mkTable instanceTypes =
  Table
    (Group NoLine [])
    (Group SingleLine [Header "instance type id", Header "monthly cost"])
    (fmap mkRow instanceTypes)

mkRow :: (Text, ProductServer) -> [Text]
mkRow (instType, prod) =
  [ instType
  , "€ " <> pack (printf "% 5.2f" prod.monthlyPrice)
  ]

-- mkTable :: [(Text, ProductServer)] -> Table Text Text Text
-- mkTable instanceTypes =
--   let rows = fmap mkRow instanceTypes
--       tableHeader =
--         Tabular.empty ^..^ colH "instance type identifier" ^|^ colH "monthly cost"
--       table = foldl' (+.+) tableHeader rows
--   in
--   table
--
-- mkRow :: (Text, ProductServer) -> SemiTable Text Text
-- mkRow (instType, prod) = row instType prodInfos
--   where
--     prodInfos :: [Text]
--     prodInfos =
--       [ "€ " <> pack (printf "% 5.2f" prod.monthlyPrice)
--       ]
  -- { monthlyPrice :: Float
  -- , ncpus :: Int
  -- , ram :: Int
  -- , arch :: Text
  -- , sumInternetBandwidth :: Int

-- example2 =
--   empty ^..^ colH "memtest 1" ^|^ colH "memtest 2"
--         ^||^ colH "time test" ^|^ colH "time test 2"
--   +.+ row "A 1" ["hog", "terrible", "slow", "slower"]
--   +.+ row "A 2" ["pig", "not bad", "fast", "slowest"]
--   +----+
--       row "B 1" ["good", "awful", "intolerable", "bearable"]
--   +.+ row "B 2" ["better", "no chance", "crawling", "amazing"]
--   +.+ row "B 3" ["meh",  "well...", "worst ever", "ok"]
