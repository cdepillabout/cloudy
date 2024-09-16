
module Cloudy.Path where

import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig, XdgData), createDirectoryIfMissing)
import System.FilePath ( (</>) )

getCloudyConfDir :: IO FilePath
getCloudyConfDir = do
  cloudyConfDirLocal <- getXdgDirectory XdgConfig "cloudy"
  createDirectoryIfMissing True cloudyConfDirLocal
  pure cloudyConfDirLocal

getCloudyConfFilePath :: IO FilePath
getCloudyConfFilePath = do
  cloudyConfDirLocal <- getCloudyConfDir
  pure $ cloudyConfDirLocal </> "cloudy.yaml"

getCloudyInstanceSetupsDir :: IO FilePath
getCloudyInstanceSetupsDir = do
  cloudyConfDirLocal <- getCloudyConfDir
  let instanceSetupsDir = cloudyConfDirLocal </> "instance-setups"
  createDirectoryIfMissing False instanceSetupsDir
  pure instanceSetupsDir

getCloudyDbPath :: IO FilePath
getCloudyDbPath = do
  cloudyStateDirLocal <- getXdgDirectory XdgData "cloudy"
  createDirectoryIfMissing True cloudyStateDirLocal
  pure $ cloudyStateDirLocal </> "db.sqlite3"

