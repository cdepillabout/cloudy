
module Cloudy.Cli.CreateScaleway where

import Cloudy.Cli.Utils (maybeOpt)
import Data.Text (Text)
import Options.Applicative

data ScalewayCliOpts = ScalewayCliOpts
  { zone :: Maybe Text
  , instanceType :: Maybe Text
  }
  deriving stock Show

scalewayCliOptsParser :: Parser ScalewayCliOpts
scalewayCliOptsParser = ScalewayCliOpts <$> zoneParser <*> instanceTypeParser

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
