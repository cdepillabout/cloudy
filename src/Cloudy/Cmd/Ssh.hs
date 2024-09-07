{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Ssh where

import Cloudy.Cli (SshCliOpts (..))
import Cloudy.Cmd.Utils (SelectInstBy, findInstanceInfoForSelectInstBy, mkSelectInstBy)
import Cloudy.LocalConfFile (LocalConfFileOpts (..))
import Cloudy.Db (withCloudyDb, InstanceInfo (..), ScalewayInstance (..))
import Data.Void (absurd)

data SshSettings = SshSettings
  { selectInstBy :: SelectInstBy
  }
  deriving stock Show

mkSettings :: LocalConfFileOpts -> SshCliOpts -> IO SshSettings
mkSettings _localConfFileOpts cliOpts = do
  selectInstBy <- mkSelectInstBy cliOpts.id cliOpts.name
  pure SshSettings { selectInstBy }

runSsh :: LocalConfFileOpts -> SshCliOpts -> IO ()
runSsh localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  ipAddr <- withCloudyDb $ \conn -> do
    instanceInfo <- findInstanceInfoForSelectInstBy conn settings.selectInstBy
    case instanceInfo of
      CloudyAwsInstance _cloudyInstance void -> absurd void
      CloudyScalewayInstance _cloudyInstance scalewayInstance -> pure scalewayInstance.scalewayIpAddress
  print ipAddr
