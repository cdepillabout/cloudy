
module Cloudy.Cmd.Destroy where

import Cloudy.Cli (DestroyCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts)

runDestroy :: LocalConfFileOpts -> DestroyCliOpts -> IO ()
runDestroy localConfFileOpts opts = pure ()
