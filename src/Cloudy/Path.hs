
module Cloudy.Path where

import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig, XdgData), createDirectoryIfMissing)
import System.FilePath ( (</>) )

getCloudyConfFilePath :: IO FilePath
getCloudyConfFilePath = do
  cloudyConfDirLocal <- getXdgDirectory XdgConfig "cloudy"
  createDirectoryIfMissing True cloudyConfDirLocal
  pure $ cloudyConfDirLocal </> "cloudy.yaml"

getCloudyDbPath :: IO FilePath
getCloudyDbPath = do
  cloudyStateDirLocal <- getXdgDirectory XdgData "cloudy"
  createDirectoryIfMissing True cloudyStateDirLocal
  pure $ cloudyStateDirLocal </> "db.sqlite3"
