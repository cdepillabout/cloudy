
module Cloudy.Cli.Aws where

import Options.Applicative (Parser, command, info, progDesc, hsubparser)

data AwsCliOpts
  = AwsCreate AwsCreateCliOpts
  | AwsListInstanceTypes AwsListInstanceTypesCliOpts
  deriving stock Show

data AwsCreateCliOpts = AwsCreateCliOpts
  deriving stock Show

data AwsListInstanceTypesCliOpts = AwsListInstanceTypesCliOpts
  deriving stock Show

awsCliOptsParser :: Parser AwsCliOpts
awsCliOptsParser = hsubparser subParsers
  where
    subParsers = createCommand <> listInstanceTypesCommand

    createCommand =
      command
        "create"
        ( info
            (fmap AwsCreate awsCreateCliOptsParser)
            (progDesc "Create a new compute instance in AWS")
        )

    listInstanceTypesCommand =
      command
        "list-instance-types"
        ( info
            (fmap AwsListInstanceTypes awsListInstanceTypesCliOptsParser)
            (progDesc "List all instance types in AWS")
        )

awsCreateCliOptsParser :: Parser AwsCreateCliOpts
awsCreateCliOptsParser = pure AwsCreateCliOpts

awsListInstanceTypesCliOptsParser :: Parser AwsListInstanceTypesCliOpts
awsListInstanceTypesCliOptsParser = pure AwsListInstanceTypesCliOpts
