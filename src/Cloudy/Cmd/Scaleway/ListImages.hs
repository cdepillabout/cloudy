{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.ListImages where

import Cloudy.Cli.Scaleway (ScalewayListImagesCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, getZone, runScalewayClientM, fetchPagedApi)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Scaleway (Zone (..), PerPage (PerPage), imagesGetApi, ImagesResp (ImagesResp), Image (..))
import Cloudy.Table (printTable, Table (..), Align (..))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text, isInfixOf, pack, toLower)
import Servant.Client (ClientM)
import Data.Time (formatTime, defaultTimeLocale)

data ScalewayListImagesSettings = ScalewayListImagesSettings
  { secretKey :: Text
  , zone :: Zone
  , arch :: Text
  , nameFilter :: Maybe Text
  }

mkSettings :: LocalConfFileOpts -> ScalewayListImagesCliOpts -> IO ScalewayListImagesSettings
mkSettings localConfFileOpts cliOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeZoneFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultZone
  zone <- getZone maybeZoneFromConfFile cliOpts.zone
  pure ScalewayListImagesSettings { secretKey, zone, arch = cliOpts.arch, nameFilter = cliOpts.nameFilter }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runListImages :: LocalConfFileOpts -> ScalewayListImagesCliOpts -> IO ()
runListImages localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  images <-
    runScalewayClientM
      (\err -> error $ "Problem fetching instance types: " <> show err)
      (fetchImages settings)
  displayImages settings images

fetchImages :: ScalewayListImagesSettings -> ClientM [Image]
fetchImages settings = do
  let authReq = createAuthReq settings.secretKey
      numPerPage = 100
  ImagesResp images <-
    fetchPagedApi
      (imagesGetApi authReq settings.zone (Just settings.arch) (Just $ PerPage numPerPage))
      (\(ImagesResp images1) (ImagesResp images2) -> ImagesResp $ images1 <> images2)
      (\(ImagesResp images) -> length images)
  pure images

displayImages :: ScalewayListImagesSettings -> [Image] -> IO ()
displayImages settings images = do
  let filteredImages =
        case settings.nameFilter of
          Nothing -> images
          Just name -> filter (\img -> isInfixOf (toLower name) (toLower img.name)) images
      sortByModDateImages = sortOn (\img -> img.modificationDate) filteredImages
  case sortByModDateImages of
    [] -> undefined
    (hImg : tImg) -> do
      let imgTable = mkTable (hImg :| tImg)
      printTable imgTable

mkTable :: NonEmpty Image -> Table
mkTable images =
  Table
    { tableHeaders =
        (LeftJustified, "image id") :|
        [ (LeftJustified, "name")
        , (LeftJustified, "arch")
        , (LeftJustified, "modify date")
        -- , (LeftJustified, "create date")
        , (LeftJustified, "state")
        ]
    , tableBodyRows = fmap mkRow images
    }

mkRow :: Image -> NonEmpty Text
mkRow img =
  img.id :|
  [ img.name
  , img.arch
  , formatDate img.modificationDate
  -- , formatDate img.creationDate
  , img.state
  ]
  where
    formatDate = pack . formatTime defaultTimeLocale "%Y-%m-%d"
