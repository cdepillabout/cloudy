
module Cloudy.Cmd.List where

import Cloudy.Cli (ListCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts)

runList :: LocalConfFileOpts -> ListCliOpts -> IO ()
runList localConfFileOpts opts = pure ()
