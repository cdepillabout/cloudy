{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.ListInstanceTypes where

import Cloudy.Cli.Scaleway (ScalewayListInstanceTypesCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, scalewayBaseUrl, getZone)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Scaleway (Zone (..), productsServersPostApi, ProductServersResp (..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, runClientM, ClientM)
import Text.Pretty.Simple (pPrint)
import Control.Monad (when)

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
  res <- runClientM (go settings) clientEnv
  pPrint res
  where
    go :: ScalewayListInstanceTypesSettings -> ClientM ()
    go settings = do
      let authReq = createAuthReq settings.secretKey
          numPerPage = 100
      ProductServersResp productServers <- productsServersPostApi authReq settings.zone (Just numPerPage)
      let numProductServers = length $ Map.elems productServers
      when (numProductServers == numPerPage) $
        liftIO $ putStrLn "WARNING: The number of instance types returned is equal to the max per page.  PROPER PAGING NEEDS TO BE IMPLEMENTED! We are likely missing instance types...."
      -- liftIO $ putStrLn "products servers resp: "
      -- pPrint vals
