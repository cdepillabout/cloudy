
module Cloudy.Cli.Scaleway where

import Data.Text (Text)
import Options.Applicative (Parser, command, info, progDesc, hsubparser, strOption, long, short, metavar)
import Cloudy.Cli.Utils (maybeOpt)

data ScalewayCliOpts
  = ScalewayCreate ScalewayCreateCliOpts
  | ScalewayListInstanceTypes ScalewayListInstanceTypesCliOpts
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

scalewayCliOptsParser :: Parser ScalewayCliOpts
scalewayCliOptsParser = hsubparser subParsers
  where
    subParsers = createCommand <> listInstanceTypesCommand

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

scalewayCreateCliOptsParser :: Parser ScalewayCreateCliOpts
scalewayCreateCliOptsParser = ScalewayCreateCliOpts <$> zoneParser <*> instanceTypeParser

scalewayListInstanceTypesCliOptsParser :: Parser ScalewayListInstanceTypesCliOpts
scalewayListInstanceTypesCliOptsParser = ScalewayListInstanceTypesCliOpts <$> zoneParser

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
