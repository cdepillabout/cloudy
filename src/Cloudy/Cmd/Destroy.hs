{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Destroy where

import Cloudy.Cli (DestroyCliOpts (..))
import Cloudy.LocalConfFile (LocalConfFileOpts)
import Cloudy.Db (CloudyInstanceId, withCloudyDb)
import Data.Text (Text)

data DestroyBy = DestroyByName Text | DestroyByInstanceId CloudyInstanceId | DestroyOnlyOne
  deriving stock Show

data DestroySettings = DestroySettings
  { destroyBy :: DestroyBy
  }
  deriving stock Show

mkSettings :: LocalConfFileOpts -> DestroyCliOpts -> IO DestroySettings
mkSettings _localConfFileOpts cliOpts = do
  let destroyBy =
        case (cliOpts.id, cliOpts.name) of
          (Just cloudyInstanceId, Nothing) -> DestroyByInstanceId cloudyInstanceId
          (Nothing, Just cloudyInstanceName) -> DestroyByName cloudyInstanceName
          (Nothing, Nothing) -> DestroyOnlyOne
          (_, _) -> error "Both cloudy instance id and cloudy instance name were specified.  You can only specify at most one of these."
  pure DestroySettings { destroyBy }

runDestroy :: LocalConfFileOpts -> DestroyCliOpts -> IO ()
runDestroy localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  print settings
  withCloudyDb $ \conn -> do
    undefined
    -- (cloudyInstanceId, instanceName) <- newCloudyInstance conn
    -- currentTime <- getCurrentTime
    -- (scalewayServerId, scalewayIpId, scalewayIpAddr) <- runScalewayClientM
    --   (\err -> error $ "ERROR! Problem creating instance: " <> show err)
    --   (createScalewayServer settings instanceName)
    -- newScalewayInstance
    --   conn
    --   currentTime
    --   cloudyInstanceId
    --   (unServerId scalewayServerId)
    --   (unIpId scalewayIpId)
    --   scalewayIpAddr
    -- putStrLn "Waiting for Scaleway instance to become available..."
    -- runScalewayClientM
    --   (\err -> error $ "ERROR! Problem waiting for instance to be ready: " <> show err)
    --   (waitForScalewayServer settings scalewayServerId)
    -- putStrLn "Scaleway instance now available."
