
module Cloudy.Cmd where

import Cloudy.Cli (CliCmd (..))
import Cloudy.Cmd.Aws (runAws)
import Cloudy.Cmd.List (runList)
import Cloudy.Cmd.Scaleway (runScaleway)
import Cloudy.Cmd.Ssh (runSsh)
import Cloudy.Cmd.Destroy (runDestroy)
import Cloudy.LocalConfFile (LocalConfFileOpts)

runCmd :: LocalConfFileOpts -> CliCmd -> IO ()
runCmd localConfFileOpts = \case
  Aws awsCliOpts -> runAws localConfFileOpts awsCliOpts
  List listCliOpts -> runList localConfFileOpts listCliOpts
  Scaleway scalewayCliOpts -> runScaleway localConfFileOpts scalewayCliOpts
  Ssh sshCliOpts -> runSsh localConfFileOpts sshCliOpts
  Destroy destroyCliOpts -> runDestroy localConfFileOpts destroyCliOpts
