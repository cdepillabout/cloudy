module Cloudy where

import Cloudy.Cli (CliCmd(..), parseCliOpts)
import Cloudy.Cmd.Create (runCreate)
import Cloudy.Cmd.List (runList)
import Cloudy.Cmd.Ssh (runSsh)
import Cloudy.Cmd.Destroy (runDestroy)
import Cloudy.LocalConfFile (readLocalConfFile)

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  localConfFileOpts <- readLocalConfFile
  case cmd of
    Create createOpts -> runCreate localConfFileOpts createOpts
    List listOpts -> runList localConfFileOpts listOpts
    Ssh sshOpts -> runSsh localConfFileOpts sshOpts
    Destroy destroyOpts -> runDestroy localConfFileOpts destroyOpts
