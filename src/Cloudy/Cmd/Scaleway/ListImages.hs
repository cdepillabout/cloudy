{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.ListImages where

import Cloudy.Cli.Scaleway (ScalewayListImagesCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, getZone, runScalewayClientM, fetchPagedApi)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Scaleway (Zone (..), PerPage (PerPage), imagesGetApi, ImagesResp (ImagesResp), Image (..))
import Cloudy.Table (printTable, Table (..), Align (..))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), groupAllWith)
import Data.Text (Text, isInfixOf, pack, toLower)
import Servant.Client (ClientM)
import Data.Time (formatTime, defaultTimeLocale)
import qualified Data.List.NonEmpty as NE

data ScalewayListImagesSettings = ScalewayListImagesSettings
  { secretKey :: Text
  , zone :: Zone
  , arch :: Text
  , nameFilter :: Maybe Text
  , showAllVersions :: Bool
  }

mkSettings :: LocalConfFileOpts -> ScalewayListImagesCliOpts -> IO ScalewayListImagesSettings
mkSettings localConfFileOpts cliOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeZoneFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultZone
  zone <- getZone maybeZoneFromConfFile cliOpts.zone
  pure
    ScalewayListImagesSettings
      { secretKey
      , zone
      , arch = cliOpts.arch
      , nameFilter = cliOpts.nameFilter
      , showAllVersions = cliOpts.allVersions
      }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runListImages :: LocalConfFileOpts -> ScalewayListImagesCliOpts -> IO ()
runListImages localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  imgs <-
    runScalewayClientM
      (\err -> error $ "Problem fetching instance types: " <> show err)
      (fetchImages settings)
  displayImages settings imgs

fetchImages :: ScalewayListImagesSettings -> ClientM [Image]
fetchImages settings = do
  let authReq = createAuthReq settings.secretKey
      numPerPage = 100
  ImagesResp imgs <-
    fetchPagedApi
      (imagesGetApi authReq settings.zone (Just settings.arch) (Just $ PerPage numPerPage))
      (\(ImagesResp images1) (ImagesResp images2) -> ImagesResp $ images1 <> images2)
      (\(ImagesResp imgs) -> length imgs)
  pure imgs

displayImages :: ScalewayListImagesSettings -> [Image] -> IO ()
displayImages settings imgs = do
  let nameFilteredImages =
        case settings.nameFilter of
          Nothing -> imgs
          Just name -> filter (\img -> isInfixOf (toLower name) (toLower img.name)) imgs
      volFilteredImages = filter (\img -> img.rootVolType == "unified") nameFilteredImages
      latestImages =
        if settings.showAllVersions
          then volFilteredImages
          else
            nubByNameArch volFilteredImages
      sortByModDateImages = sortOn (\img -> img.modificationDate) latestImages
  case sortByModDateImages of
    [] -> undefined
    (hImg : tImg) -> do
      let imgTable = mkTable (hImg :| tImg)
      printTable imgTable

nubByNameArch :: [Image] -> [Image]
nubByNameArch imgs =
  fmap getMostRecent $ groupAllWith (\img -> (img.name, img.arch)) imgs
  where
    getMostRecent :: NonEmpty Image -> Image
    getMostRecent = NE.head . NE.sortWith (\img -> img.modificationDate)

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
