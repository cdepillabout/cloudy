
module Cloudy.Cmd.Ssh where

import Cloudy.Cli (SshCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts)

runSsh :: LocalConfFileOpts -> SshCliOpts -> IO ()
runSsh localConfFileOpts opts = pure ()
