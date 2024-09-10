{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.CopyFile where

import Cloudy.Cli (CopyFileCliOpts (..))
import Cloudy.Cmd.Utils (SelectInstBy, findInstanceInfoForSelectInstBy, mkSelectInstBy)
import Cloudy.LocalConfFile (LocalConfFileOpts (..))
import Cloudy.Db (withCloudyDb, InstanceInfo (..), ScalewayInstance (..))
import Data.Text (unpack)
import Data.Void (absurd)
import System.Posix.Process (executeFile)

data CopyFileSettings = CopyFileSettings
  { selectInstBy :: SelectInstBy
  }
  deriving stock Show

mkSettings :: LocalConfFileOpts -> CopyFileCliOpts -> IO CopyFileSettings
mkSettings _localConfFileOpts cliOpts = do
  selectInstBy <- mkSelectInstBy cliOpts.id cliOpts.name
  pure CopyFileSettings { selectInstBy }

runCopyFile :: LocalConfFileOpts -> CopyFileCliOpts -> IO ()
runCopyFile localConfFileOpts cliOpts = do
  settings <- mkSettings localConfFileOpts cliOpts
  ipAddr <- withCloudyDb $ \conn -> do
    instanceInfo <- findInstanceInfoForSelectInstBy conn settings.selectInstBy
    case instanceInfo of
      CloudyAwsInstance _cloudyInstance void -> absurd void
      CloudyScalewayInstance _cloudyInstance scalewayInstance -> pure scalewayInstance.scalewayIpAddress
  let copyFileArgs = fmap unpack $ "root@" <> ipAddr : cliOpts.passthru
  -- executeFile "copyFile" True sshArgs Nothing
