
module Cloudy.Cmd.Scaleway where

import Cloudy.Cli (ScalewayCliOpts (..))
import Cloudy.Cmd.Scaleway.Create (runCreate)
import Cloudy.Cmd.Scaleway.ListInstanceTypes (runListInstanceTypes)
import Cloudy.LocalConfFile (LocalConfFileOpts)

runScaleway :: LocalConfFileOpts -> ScalewayCliOpts -> IO ()
runScaleway localConfFileOpts = \case
  ScalewayCreate scalewayCreateCliOpts -> runCreate localConfFileOpts scalewayCreateCliOpts
  ScalewayListInstanceTypes scalewayListInstanceTypesCliOpts -> runListInstanceTypes localConfFileOpts scalewayListInstanceTypesCliOpts
