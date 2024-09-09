{-# LANGUAGE OverloadedRecordDot #-}

module Cloudy.Cmd.Scaleway.Create where

import Cloudy.Cli.Scaleway (ScalewayCreateCliOpts (..))
import Cloudy.Cmd.Scaleway.Utils (createAuthReq, getZone, runScalewayClientM, getInstanceType, getImageId)
import Cloudy.LocalConfFile (LocalConfFileOpts (..), LocalConfFileScalewayOpts (..))
import Cloudy.Db (newCloudyInstance, newScalewayInstance, withCloudyDb)
import Cloudy.Scaleway (ipsPostApi, Zone (..), IpsReq (..), IpsResp (..), ProjectId (..), serversPostApi, ServersReq (..), ServersResp (..), ImageId (ImageId), serversUserDataPatchApi, UserDataKey (UserDataKey), UserData (UserData), ServersActionReq (..), serversActionPostApi, ServersRespVolume (..), ServersReqVolume (..), VolumesReq (..), volumesPatchApi, ServerId, unServerId, serversGetApi, IpId, unIpId, zoneToText, serversUserDataGetApi)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Data.Time (getCurrentTime)
import Servant.Client (ClientM)
import Servant.API (NoContent(NoContent))
import Control.Monad (when)
import Network.Socket (AddrInfo(..), SocketType (Stream), defaultHints, getAddrInfo, openSocket, gracefulClose, connect)
import Control.Exception (bracket, SomeException, try)
import Control.Concurrent (threadDelay)

data ScalewayCreateSettings = ScalewayCreateSettings
  { secretKey :: Text
  , projectId :: ProjectId
  , zone :: Zone
  , instanceType :: Text
  , volumeSizeGb :: Int
  , imageId :: Text
  }

mkSettings :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ScalewayCreateSettings
mkSettings localConfFileOpts cliOpts = do
  let maybeSecretKey = localConfFileOpts.scaleway >>= \scale -> scale.secretKey :: Maybe Text
  secretKey <- getVal maybeSecretKey "Could not find scaleway.secret_key in config file"
  let maybeProjectId = localConfFileOpts.scaleway >>= \scale -> fmap ProjectId scale.defaultProjectId
  projectId <- getVal maybeProjectId "Could not find scaleway.default_project_id in config file"
  let maybeZoneFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultZone
  zone <- getZone maybeZoneFromConfFile cliOpts.zone
  let maybeInstanceTypeFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultInstanceType
      instanceType = getInstanceType maybeInstanceTypeFromConfFile cliOpts.instanceType
  let maybeImageIdFromConfFile = localConfFileOpts.scaleway >>= \scale -> scale.defaultImageId
      imageId = getImageId maybeImageIdFromConfFile cliOpts.imageId
  pure
    ScalewayCreateSettings
      { secretKey
      , projectId
      , zone
      , instanceType
      , volumeSizeGb = cliOpts.volumeSizeGb
      , imageId
      }
  where
    getVal :: Maybe a -> String -> IO a
    getVal mayVal errMsg = maybe (error errMsg) pure mayVal

runCreate :: LocalConfFileOpts -> ScalewayCreateCliOpts -> IO ()
runCreate localConfFileOpts scalewayOpts = do
  settings <- mkSettings localConfFileOpts scalewayOpts
  withCloudyDb $ \conn -> do
    (cloudyInstanceId, instanceName) <- newCloudyInstance conn
    currentTime <- getCurrentTime
    (scalewayServerId, scalewayIpId, scalewayIpAddr) <- runScalewayClientM
      (\err -> error $ "ERROR! Problem creating instance: " <> show err)
      (createScalewayServer settings instanceName)
    newScalewayInstance
      conn
      currentTime
      cloudyInstanceId
      (zoneToText settings.zone)
      (unServerId scalewayServerId)
      (unIpId scalewayIpId)
      scalewayIpAddr
    putStrLn "Waiting for Scaleway instance to become available..."
    runScalewayClientM
      (\err -> error $ "ERROR! Problem waiting for instance to be ready: " <> show err)
      (waitForScalewayServer settings scalewayServerId)
    putStrLn "Scaleway instance now available."
    putStrLn "Waiting for SSH to be ready on the instance..."
    waitForSshPort scalewayIpAddr
    putStrLn "SSH now available on the instance."
    putStrLn "Getting instance SSH key fingerprints..."
    rawSshKeyFingerprintsFromScalewayApi <- runScalewayClientM
      (\err -> error $ "ERROR! Problem getting instance SSH key fingerprints: " <> show err)
      (getSshKeyFingerprints settings scalewayServerId)
    -- TODO: parse ssh key fingerprints and update ~/.ssh/known_hosts
    updateSshHostKeys rawSshKeyFingerprintsFromScalewayApi scalewayIpAddr


createScalewayServer :: ScalewayCreateSettings -> Text -> ClientM (ServerId, IpId, Text)
createScalewayServer settings instanceName = do
  let authReq = createAuthReq settings.secretKey
  ipsResp <- ipsPostApi authReq settings.zone (IpsReq "routed_ipv4" settings.projectId)
  liftIO $ putStrLn $ "ips resp: " <> show ipsResp
  let serversReq =
        ServersReq
          { bootType = "local"
          , commercialType = settings.instanceType
          , image = ImageId settings.imageId
          , name = "cloudy-" <> instanceName
          , publicIps = [ipsResp.id]
          , tags = ["cloudy"]
          , volumes =
              Map.fromList
                [ ( "0"
                  , ServersReqVolume
                      { size = settings.volumeSizeGb * oneGb
                      , volumeType = "b_ssd"
                      }
                  )
                ]
          , project = settings.projectId
          }
  serversResp <- serversPostApi authReq settings.zone serversReq
  liftIO $ putStrLn $ "servers resp: " <> show serversResp
  let maybeFirstVol = Map.lookup "0" serversResp.volumes
  firstVol <- maybe (error "couldn't find first volume, unexpected") pure maybeFirstVol
  -- The volume's name has to initial be created as empty (""), and only
  -- after that can we set the name separately.
  serversVolumesResp <-
    volumesPatchApi
      authReq
      settings.zone
      firstVol.id
      (VolumesReq $ "cloudy-" <> instanceName <> "-boot-block-volume")
  liftIO $ putStrLn $ "servers volumes resp: " <> show serversVolumesResp
  let userData =
        unlines
          [ "#!/usr/bin/env bash"
          , "echo 'hello' >> /whatwhat"
          ]
  NoContent <-
    serversUserDataPatchApi
      authReq
      settings.zone
      serversResp.id
      (UserDataKey "cloud-init")
      (UserData $ pack userData)
  liftIO $ putStrLn "created user data"
  let act = ServersActionReq { action = "poweron" }
  task <- serversActionPostApi authReq settings.zone serversResp.id act
  liftIO $ print task
  pure (serversResp.id, ipsResp.id, ipsResp.address)

oneGb :: Int
oneGb = 1000 * 1000 * 1000

waitForScalewayServer :: ScalewayCreateSettings -> ServerId -> ClientM ()
waitForScalewayServer settings serverId = do
  let authReq = createAuthReq settings.secretKey
  serversResp <- serversGetApi authReq settings.zone serverId
  when (serversResp.state /= "running") $ waitForScalewayServer settings serverId

-- | Wait for port 22 to be available on the remote machine.
waitForSshPort :: Text -> IO ()
waitForSshPort ipaddrText = do
  let hints = defaultHints { addrSocketType = Stream }
  addrInfos <- getAddrInfo (Just hints) (Just $ unpack ipaddrText) (Just "22")
  case addrInfos of
    [] -> error "Couldn't get addr info for instance"
    addrInfo : _ -> tryConnect addrInfo
  where
    tryConnect :: AddrInfo -> IO ()
    tryConnect addrInfo = do
      res <-
        try $ bracket (openSocket addrInfo) (`gracefulClose` 1000) $ \sock ->
          connect sock $ addrAddress addrInfo
      case res of
        Left (_ :: SomeException) -> do
          threadDelay 1_000_000
          tryConnect addrInfo
        Right _ -> pure ()

getSshKeyFingerprints :: ScalewayCreateSettings -> ServerId -> ClientM Text
getSshKeyFingerprints settings serverId = do
  let authReq = createAuthReq settings.secretKey
  UserData rawSshKeyFingerprints <- serversUserDataGetApi authReq settings.zone serverId (UserDataKey "ssh-host-fingerprints")
  pure rawSshKeyFingerprints

-- example ssh key fingerprints user data file:
--
-- 3072 SHA256:dRJ/XiNOlh9UGnnN5/a2N+EMSP+OkqyHy8WTzHlUt5U root@cloudy-complete-knife (RSA)
-- 256 SHA256:n6fLRD4O2Me3bRXhzHyCca1vWdQ2utxuPZVsIDUm6o0 root@cloudy-complete-knife (ECDSA)
-- 256 SHA256:PMESLB3kYYV/8YHS/5Q3wLdufjqhZ/flkQolLIth/KE root@cloudy-complete-knife (ED25519)

updateSshHostKeys ::
  Text ->
  -- | IP Address
  Text ->
  IO ()
updateSshHostKeys rawFingerprintsFromScalewayApi ipAddr = do
  fingerprintsFromScalewayApi <-
    fromEitherM
      (\parseErr ->
          error $
            "Error parsing SSH host fingerprints from Scaleway metadata api: " <>
            show parseErr
      )
      (parseFingerprints "scaleway-metadata-api" rawFingerprintsFromScalewayApi)
  rawHostKeys <- getSshHostKeys ipAddr
  rawFingerprintsFromHost <- fingerprintsFromHostKeys rawSshHostKeys
  fingerprintsFromHost <-
    fromEitherM
      (\parseErr ->
          error $
            "Error parsing SSH host fingerprints directly from instance: " <>
            show parseErr
      )
      (parseFingerprints "host" rawFingerprintsFromScalewayApi)
  if doFingerprintsMatch fingerprintsFromScalewayApi fingerprintsFromHost
    then do
      removeOldHostKeysFromKnownHosts ipAddr
      addNewHostKeysToKnownHosts rawHostKeys
    else do
      error $
        "ERROR: Fingerprints from scaleway metadata api, and fingerprints " <>
        "directly from host don't match.\n\nFrom metadata api:\n\n" <>
        rawFingerprintsFromScalewayApi <>
        "\n\nFrom host: \n\n" <>
        rawFingerprintsFromHost


-- | This datatype represents a line from an SSH fingerprint file, normally as
-- output by @ssh-keygen -l@.
--
-- Here's an example line:
--
-- > 3072 SHA256:dRJ/XiNOlh9UGnnN5/a2N+EMSP+OkqyHy8WTzHlUt5U root@cloudy-complete-knife (RSA)
data Fingerprint = Fingerpint
  { size :: Int
    -- ^ Size of the key.  Example: @3072@
  , fingerprint :: Text
    -- ^ The fingerprint of the key.  Example: @"SHA256:n6fLRD4O2Me3bRXhzHyCca1vWdQ2utxuPZVsIDUm6o0"@
  , fingerServer :: Text
    -- ^ User and hostname.  Example: @"root\@cloudy-complete-knife"@
  , fingerKeyType :: Text
    -- ^ Type of key.  Example: @"RSA"@
  }

parseFingerprints ::
  -- | Where are these fingerprints coming from?
  --
  -- Just used in error output to help debugging.
  Text ->
  -- | The raw fingerprint file.  See 'Fingerprint' for what a single line of
  -- this file looks like.  The whole file is just multiple of these lines,
  -- separate by a new line.
  Text ->
  Either ParseError [Fingerprint]
parseFingerprints fromWhere rawFingerpintText = do
  parse fingerprintsParser fromWhere rawFingerprintText
