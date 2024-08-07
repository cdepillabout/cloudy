
module Cloudy.Cli.CreateScaleway where

import Options.Applicative

data ScalewayCliOpts = ScalewayCliOpts
  deriving stock Show

scalewayCliOptsParser :: Parser ScalewayCliOpts
scalewayCliOptsParser = pure ScalewayCliOpts
