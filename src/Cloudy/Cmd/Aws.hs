
module Cloudy.Cmd.Aws where

import Cloudy.LocalConfFile (LocalConfFileOpts)
import Cloudy.Cli (AwsCliOpts (..))
import Cloudy.Cli.Aws (AwsCreateCliOpts (..), AwsListInstanceTypesCliOpts (..))

runAws :: LocalConfFileOpts -> AwsCliOpts -> IO ()
runAws localConfFileOpts = \case
  AwsCreate awsCreateCliOpts -> runAwsCreate localConfFileOpts awsCreateCliOpts
  AwsListInstanceTypes awsListInstanceTypesCliOpts -> runAwsListInstanceTypes localConfFileOpts awsListInstanceTypesCliOpts

runAwsCreate :: LocalConfFileOpts -> AwsCreateCliOpts -> IO ()
runAwsCreate localConfFileOpts createCliOpts = undefined

runAwsListInstanceTypes :: LocalConfFileOpts -> AwsListInstanceTypesCliOpts -> IO ()
runAwsListInstanceTypes localConfFileOpts listInstanceTypesCliOpts = undefined
