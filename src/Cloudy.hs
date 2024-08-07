module Cloudy where

import Cloudy.Cli (CliCmd(..), parseCliOpts)
import Cloudy.Cmd.Create (runCreate)
import Cloudy.Cmd.List (runList)
import Cloudy.Cmd.Ssh (runSsh)
import Cloudy.Cmd.Destroy (runDestroy)

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  case cmd of
    Create createOpts -> runCreate createOpts
    List listOpts -> runList listOpts
    Ssh sshOpts -> runSsh sshOpts
    Destroy destroyOpts -> runDestroy destroyOpts
