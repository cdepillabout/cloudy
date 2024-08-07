
module Cloudy.Cli.CreateAws where

import Options.Applicative

data AwsCliOpts = AwsCliOpts
  deriving stock Show

awsCliOptsParser :: Parser AwsCliOpts
awsCliOptsParser = pure AwsCliOpts
