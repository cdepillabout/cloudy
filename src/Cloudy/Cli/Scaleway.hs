{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cli.Scaleway where

import Cloudy.Cli.Utils (maybeOpt)
import Cloudy.InstanceSetup (builtInInstanceSetups)
import Cloudy.InstanceSetup.Types (InstanceSetup (..), InstanceSetupData (..))
import Control.Applicative (optional)
import Data.Text (Text, unpack)
import Options.Applicative (Parser, command, info, progDesc, hsubparser, strOption, long, short, metavar, option, help, value, showDefault, maybeReader, switch, auto, footerDoc, completeWith)
import Options.Applicative.Help (vsep, Doc)
import Data.String (IsString(fromString))
import Cloudy.Scaleway (allScalewayZones, zoneToText)

data ScalewayCliOpts
  = ScalewayCreate ScalewayCreateCliOpts
  | ScalewayListInstanceTypes ScalewayListInstanceTypesCliOpts
  | ScalewayListImages ScalewayListImagesCliOpts
  deriving stock Show

data ScalewayCreateCliOpts = ScalewayCreateCliOpts
  { zone :: Maybe Text
  , instanceType :: Maybe Text
  , volumeSizeGb :: Int
  , imageId :: Maybe Text
  , instanceSetup :: Maybe Text
  }
  deriving stock Show

data ScalewayListInstanceTypesCliOpts = ScalewayListInstanceTypesCliOpts
  { zone :: Maybe Text
  }
  deriving stock Show

data ScalewayListImagesCliOpts = ScalewayListImagesCliOpts
  { zone :: Maybe Text
  , arch :: Text
  , nameFilter :: Maybe Text
  , allVersions :: Bool
  }
  deriving stock Show

scalewayCliOptsParser :: [InstanceSetup] -> Parser ScalewayCliOpts
scalewayCliOptsParser userInstanceSetups = hsubparser subParsers
  where
    subParsers = createCommand <> listInstanceTypesCommand <> listImagesCommand

    createCommand =
      command
        "create"
        ( info
            (ScalewayCreate <$> scalewayCreateCliOptsParser userInstanceSetups)
            ( progDesc "Create a new compute instance in Scaleway" <>
              (footerDoc . Just $
                -- TODO: do this better
                vsep
                  ( [ "You can use the --instance-setup option to configure which \
                      \instance setup script is used to setup the instance after \
                      \boot.  The instance setup scripts generally have a \
                      \`cloud-init` section, which specifies the actual cloud-init \
                      \setup to use."
                    , ""
                    , "Default instance-setup scripts builtin to Cloudy:"
                    , ""
                    ] <>
                    fmap instanceSetupToDoc builtInInstanceSetups <>
                    [ ""
                    , "User-defined instance-setup scripts in ~/.config/cloudy/instance-setups/:"
                    , ""
                    ] <>
                    case userInstanceSetups of
                      [] -> ["(none exist)"]
                      _ -> fmap instanceSetupToDoc userInstanceSetups
                  )
              )
            )
        )

    listInstanceTypesCommand =
      command
        "list-instance-types"
        ( info
            (fmap ScalewayListInstanceTypes scalewayListInstanceTypesCliOptsParser)
            (progDesc "List all instance types in Scaleway")
        )

    listImagesCommand =
      command
        "list-images"
        ( info
            (fmap ScalewayListImages scalewayListImagesCliOptsParser)
            (progDesc "List available images in Scaleway")
        )

instanceSetupToDoc :: InstanceSetup -> Doc
instanceSetupToDoc instanceSetup =
  "    - " <> fromString (unpack instanceSetup.name) <>
  "  --  " <> fromString (unpack instanceSetup.instanceSetupData.shortDescription)

scalewayCreateCliOptsParser :: [InstanceSetup] -> Parser ScalewayCreateCliOpts
scalewayCreateCliOptsParser userInstanceSetups =
  ScalewayCreateCliOpts
    <$> zoneParser
    <*> instanceTypeParser
    <*> volumeSizeGbParser
    <*> imageIdParser
    <*> instanceSetupParser userInstanceSetups

scalewayListInstanceTypesCliOptsParser :: Parser ScalewayListInstanceTypesCliOpts
scalewayListInstanceTypesCliOptsParser = ScalewayListInstanceTypesCliOpts <$> zoneParser

scalewayListImagesCliOptsParser :: Parser ScalewayListImagesCliOpts
scalewayListImagesCliOptsParser =
  ScalewayListImagesCliOpts
    <$> zoneParser
    <*> archParser
    <*> nameFilterParser
    <*> allVersionsParser

zoneParser :: Parser (Maybe Text)
zoneParser =
  maybeOpt
    "Scaleway zone in which to create the new instance"
    "nl-ams-1"
    strOption
    ( long "zone" <>
      short 'z' <>
      metavar "ZONE" <>
      completeWith (unpack . zoneToText <$> allScalewayZones)
    )

instanceTypeParser :: Parser (Maybe Text)
instanceTypeParser =
  maybeOpt
    "Scaleway instance type (use `cloudy scaleway list-instance-types` command to get list of all instance types)"
    "PLAY2-NANO"
    strOption
    ( long "instance-type" <>
      short 'c' <>
      metavar "INSTANCE_TYPE"
    )

archParser :: Parser Text
archParser =
  option
    (maybeReader archReader)
    ( long "arch" <>
      short 'a' <>
      metavar "ARCH" <>
      help "Architecture of image.  Possiblities: \"x86_64\", \"arm\", or \"arm64\"" <>
      value "x86_64" <>
      showDefault <>
      completeWith ["x86_64", "arm", "arm64"]
    )
  where
    archReader :: String -> Maybe Text
    archReader = \case
      "x86_64" -> Just "x86_64"
      "arm" -> Just "arm"
      "arm64" -> Just "arm64"
      _ -> Nothing

nameFilterParser :: Parser (Maybe Text)
nameFilterParser =
  optional $
    strOption
      ( long "name-filter" <>
        short 'n' <>
        metavar "NAME_FILTER" <>
        help "Only show images whose name contains this value, case-insensitive (default: no filter)"
      )

allVersionsParser :: Parser Bool
allVersionsParser =
  switch
    ( long "all-versions" <>
      short 'a' <>
      help "List all versions of each image.  By default, only show the latest version for each image name."
    )

volumeSizeGbParser :: Parser Int
volumeSizeGbParser =
  option
    auto
    ( long "volume-size" <>
      short 's' <>
      metavar "VOLUME_SIZE" <>
      help "Size of the root volume in GBs" <>
      value 50 <>
      showDefault
    )

imageIdParser :: Parser (Maybe Text)
imageIdParser =
  maybeOpt
    "Scaleway image ID (use `cloudy scaleway list-images` command to get list of possible image IDs). Also can be image label, like \"ubuntu_noble\" (TODO: implement market api to return list of possible labels)"
    "ubuntu_noble"
    strOption
    ( long "image-id" <>
      short 'i' <>
      metavar "IMAGE_ID"
    )

instanceSetupParser :: [InstanceSetup] -> Parser (Maybe Text)
instanceSetupParser userInstanceSetups =
  optional $
    strOption
      ( long "instance-setup" <>
        short 't' <>
        metavar "INSTANCE_SETUP" <>
        help "Name of the instance-setup to use when booting the image.  (default: do no instance setup)" <>
        completeWith
          (fmap (\instSetup -> unpack instSetup.name) (userInstanceSetups <> builtInInstanceSetups))
      )
