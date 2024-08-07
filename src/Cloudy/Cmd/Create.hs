
module Cloudy.Cmd.Create where

import Cloudy.Cli (CreateCliOpts (..), AwsCliOpts (..), ScalewayCliOpts (..))
import Cloudy.Scaleway (ipsPostApi)

runCreate :: CreateCliOpts -> IO ()
runCreate = \case
  CreateCliOptsScaleway scalewayOpts -> runCreateScaleway scalewayOpts
  CreateCliOptsAws awsOpts -> runCreateAws awsOpts

runCreateAws :: AwsCliOpts -> IO ()
runCreateAws awsOpts = undefined

runCreateScaleway :: ScalewayCliOpts -> IO ()
runCreateScaleway scalewayOpts = undefined
