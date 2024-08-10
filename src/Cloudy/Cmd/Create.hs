module Cloudy.Cmd.Create where

import Cloudy.Cli (CreateCliOpts (..), AwsCliOpts (..))
import Cloudy.Cmd.Create.Scaleway (runCreateScaleway)
import Cloudy.LocalConfFile (LocalConfFileOpts)

runCreate :: LocalConfFileOpts -> CreateCliOpts -> IO ()
runCreate localConfFileOpts = \case
  CreateCliOptsScaleway scalewayOpts -> runCreateScaleway localConfFileOpts scalewayOpts
  CreateCliOptsAws awsOpts -> runCreateAws localConfFileOpts awsOpts

runCreateAws :: LocalConfFileOpts -> AwsCliOpts -> IO ()
runCreateAws _localConfFileOpts _awsOpts = undefined
