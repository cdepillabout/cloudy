{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Ssh where

import Cloudy.Cli (SshCliOpts (..))
import Cloudy.Cmd.Utils (SelectInstBy, findInstanceInfoForSelectInstBy, mkSelectInstBy)
import Cloudy.LocalConfFile (LocalConfFileOpts (..))
import Cloudy.Db (withCloudyDb, InstanceInfo (..), ScalewayInstance (..))
import Data.Text (unpack, Text)
import Data.Void (absurd)
import System.Posix.Process (executeFile)

data SshSettings = SshSettings
  { selectInstBy :: SelectInstBy
  , sshPassthruArgs :: [Text]
  }
  deriving stock Show

mkSettings :: LocalConfFileOpts -> SshCliOpts -> IO SshSettings
mkSettings _localConfFileOpts cliOpts = do
  selectInstBy <- mkSelectInstBy cliOpts.id cliOpts.name
  pure SshSettings { selectInstBy, sshPassthruArgs = cliOpts.passthru }

runSsh :: LocalConfFileOpts -> SshCliOpts -> IO ()
runSsh localConfFileOpts cliOpts = do
  settings <- mkSettings localConfFileOpts cliOpts
  ipAddr <- withCloudyDb $ \conn -> do
    instanceInfo <- findInstanceInfoForSelectInstBy conn settings.selectInstBy
    case instanceInfo of
      CloudyAwsInstance _cloudyInstance void -> absurd void
      CloudyScalewayInstance _cloudyInstance scalewayInstance -> pure scalewayInstance.scalewayIpAddress
  let sshArgs = fmap unpack $ "root@" <> ipAddr : settings.sshPassthruArgs
  executeFile "ssh" True sshArgs Nothing
