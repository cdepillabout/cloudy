
module Cloudy.Cli.Scaleway where

import Data.Text (Text)
import Options.Applicative (Parser, command, info, progDesc, hsubparser, strOption, long, short, metavar, option, ReadM, help, value, showDefault, maybeReader, Alternative ((<|>)))
import Cloudy.Cli.Utils (maybeOpt)

data ScalewayCliOpts
  = ScalewayCreate ScalewayCreateCliOpts
  | ScalewayListInstanceTypes ScalewayListInstanceTypesCliOpts
  | ScalewayListImages ScalewayListImagesCliOpts
  deriving stock Show

data ScalewayCreateCliOpts = ScalewayCreateCliOpts
  { zone :: Maybe Text
  , instanceType :: Maybe Text
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
  }
  deriving stock Show

scalewayCliOptsParser :: Parser ScalewayCliOpts
scalewayCliOptsParser = hsubparser subParsers
  where
    subParsers = createCommand <> listInstanceTypesCommand <> listImagesCommand

    createCommand =
      command
        "create"
        ( info
            (fmap ScalewayCreate scalewayCreateCliOptsParser)
            (progDesc "Create a new compute instance in Scaleway")
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

scalewayCreateCliOptsParser :: Parser ScalewayCreateCliOpts
scalewayCreateCliOptsParser = ScalewayCreateCliOpts <$> zoneParser <*> instanceTypeParser

scalewayListInstanceTypesCliOptsParser :: Parser ScalewayListInstanceTypesCliOpts
scalewayListInstanceTypesCliOptsParser = ScalewayListInstanceTypesCliOpts <$> zoneParser

scalewayListImagesCliOptsParser :: Parser ScalewayListImagesCliOpts
scalewayListImagesCliOptsParser = ScalewayListImagesCliOpts <$> zoneParser <*> archParser <*> nameFilterParser


zoneParser :: Parser (Maybe Text)
zoneParser =
  maybeOpt
    "Scaleway zone in which to create the new instance"
    "nl-ams-1"
    strOption
    ( long "zone" <>
      short 'z' <>
      metavar "ZONE"
    )

instanceTypeParser :: Parser (Maybe Text)
instanceTypeParser =
  maybeOpt
    "Scaleway instance type (use `cloudy scaleway list-instance-types` command to get list of all instance types)"
    "PLAY2-PICO"
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
      showDefault
    )
  where
    archReader :: String -> Maybe Text
    archReader = \case
      "x86_64" -> Just "x86_64"
      "arm" -> Just "arm"
      "arm64" -> Just "arm64"
      _ -> Nothing

nameFilterParser :: Parser (Maybe Text)
nameFilterParser = fmap Just innerParser <|> pure Nothing
  where
    innerParser =
      strOption
        ( long "name-filter" <>
          short 'n' <>
          metavar "NAME_FILTER" <>
          help "Only show images whose name contains this value, case-insensitive (default: no filter)"
        )
