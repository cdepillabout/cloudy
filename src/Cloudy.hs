module Cloudy where

import Cloudy.Cli (parseCliOpts)
import Cloudy.Cmd (runCmd)
import Cloudy.LocalConfFile (readLocalConfFile)

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  localConfFileOpts <- readLocalConfFile
  runCmd localConfFileOpts cmd
