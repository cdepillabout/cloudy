
module Cloudy.Cli.CreateScaleway where

import Cloudy.Cli.Utils (maybeOpt)
import Data.Text (Text)
import Options.Applicative

data ScalewayCliOpts = ScalewayCliOpts
  { zone :: Maybe Text
  }
  deriving stock Show

scalewayCliOptsParser :: Parser ScalewayCliOpts
scalewayCliOptsParser = ScalewayCliOpts <$> zoneParser

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
