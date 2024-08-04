module Cloudy where

import Cloudy.Cli (CliCmd(..), parseCliOpts)

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  undefined
