
module Cloudy.Cmd.Scaleway where

import Cloudy.Cli (ScalewayCliOpts (..))
import Cloudy.Cmd.Scaleway.Create (runCreate)
import Cloudy.Cmd.Scaleway.ListImages (runListImages)
import Cloudy.Cmd.Scaleway.ListInstanceTypes (runListInstanceTypes)
import Cloudy.LocalConfFile (LocalConfFileOpts)

runScaleway :: LocalConfFileOpts -> ScalewayCliOpts -> IO ()
runScaleway localConfFileOpts = \case
  ScalewayCreate scalewayCreateCliOpts -> runCreate localConfFileOpts scalewayCreateCliOpts
  ScalewayListImages scalewayListImagesCliOpts -> runListImages localConfFileOpts scalewayListImagesCliOpts
  ScalewayListInstanceTypes scalewayListInstanceTypesCliOpts -> runListInstanceTypes localConfFileOpts scalewayListInstanceTypesCliOpts
