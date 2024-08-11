
module Cloudy.Cli.CreateScaleway where

import Cloudy.Cli.Utils (maybeOpt)
import Data.Text (Text)
import Options.Applicative

data ScalewayCliOpts = ScalewayCliOpts
  { zone :: Maybe Text
  , commercialType :: Maybe Text
  }
  deriving stock Show

scalewayCliOptsParser :: Parser ScalewayCliOpts
scalewayCliOptsParser = ScalewayCliOpts <$> zoneParser <*> commercialTypeParser

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

commercialTypeParser :: Parser (Maybe Text)
commercialTypeParser =
  maybeOpt
    "Scaleway commercial type (instance type)"
    "PLAY2-PICO"
    strOption
    ( long "commercial-type" <>
      short 'c' <>
      metavar "COMMERCIAL_TYPE"
    )
