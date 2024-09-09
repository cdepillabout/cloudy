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
import Control.Monad (when, void)
import Network.Socket (AddrInfo(..), SocketType (Stream), defaultHints, getAddrInfo, openSocket, gracefulClose, connect)
import Control.Exception (bracket, SomeException, try)
import Control.Concurrent (threadDelay)
import Text.Parsec (ParseError, parse, Parsec, sepBy1, newline, space, eof, sepEndBy1, digit, char, anyChar, manyTill)
import Control.FromSum (fromEitherM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word64)
import Control.Applicative (some)
import Text.Read (readMaybe)

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
  rawFingerprintsFromHost <- fingerprintsFromHostKeys rawHostKeys
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
        unpack rawFingerprintsFromScalewayApi <>
        "\n\nFrom host: \n\n" <>
        unpack rawFingerprintsFromHost

-- | This datatype represents a line from an SSH fingerprint file, normally as
-- output by @ssh-keygen -l@.
--
-- Here's an example line:
--
-- > 3072 SHA256:dRJ/XiNOlh9UGnnN5/a2N+EMSP+OkqyHy8WTzHlUt5U root@cloudy-complete-knife (RSA)
data Fingerprint = Fingerprint
  { size :: Word64
    -- ^ Size of the key.  Example: @3072@
  , fingerprint :: Text
    -- ^ The fingerprint of the key.  Example: @"SHA256:n6fLRD4O2Me3bRXhzHyCca1vWdQ2utxuPZVsIDUm6o0"@
  , server :: Text
    -- ^ User and hostname.  Example: @"root\@cloudy-complete-knife"@
  , keyType :: Text
    -- ^ Type of key.  Example: @"RSA"@
  }
  deriving stock Show

type Parser = Parsec Text ()

parseFingerprints ::
  -- | Where are these fingerprints coming from?
  --
  -- Just used in error output to help debugging.
  Text ->
  -- | The raw fingerprint file.  See 'Fingerprint' for what a single line of
  -- this file looks like.  The whole file is just multiple of these lines,
  -- separate by a new line.
  Text ->
  Either ParseError (NonEmpty Fingerprint)
parseFingerprints fromWhere rawFingerprintText = do
  parse (fingerprintsParser <* eof) (unpack fromWhere) rawFingerprintText

fingerprintsParser :: Parser (NonEmpty Fingerprint)
fingerprintsParser = do
  fingerprints <- sepEndBy1 fingerprintParser newline
  case fingerprints of
    [] -> error "fingerprintsParser: sepEndBy1 is never expected to return empty list"
    (h : ts) -> pure $ h :| ts

-- | Parse a single 'Fingerprint'.
--
-- >>> let finger = "3072 SHA256:dRJ/XiNOlh9UGnnN5/a2N+EMSP+OkqyHy8WTzHlUt5U root@cloudy-complete-knife (RSA)"
-- >>> parseTest fingerprintParser finger
-- Fingerprint {size = 3072, fingerprint = "SHA256:dRJ/XiNOlh9UGnnN5/a2N+EMSP+OkqyHy8WTzHlUt5U", server = "root@cloudy-complete-knife", keyType = "RSA"}
fingerprintParser :: Parser Fingerprint
fingerprintParser = do
  size <- int
  void space
  fingerprint <- pack <$> manyTill anyChar space
  server <- pack <$> manyTill anyChar space
  void $ char '('
  keyType <- pack <$> manyTill anyChar (char ')')
  pure $ Fingerprint { size, fingerprint, server, keyType }
  where
    int :: Parser Word64
    int = do
      digits <- some digit
      case readMaybe digits of
        Nothing -> fail $ "Couldn't read digits as Word64: " <> digits
        Just i -> pure i

-- | Returns the SSH host keys for the given host.
--
-- This effectively just runs @ssh-keyscan@ on the given host.
--
-- This returns an output that looks like the following:
--
-- > 123.100.200.3 ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCiRtLMhK1Dh72tpJIXF+NjLAPPyXbq/tYC0ztDTMBFfQEj2jixURcugtGM7WjcqDCHHgnPDcSHrlkl9dMOV0MvjA2WxNupDU1bPQ31h10rIiiSjL+IB+c9e1wEgJylt72pDPzxDjdNfuAS3gspOjYNuy2vRBlV8rQ9GDlSoSvqMGbQ7W9bdCLnANsUkI+FCXFZCzIL3MU26ddqrBdCgiTvFUVxHjfFJMxwsKwLa18P6dc586mYXocmQGwjyXfJCiOw5kajvH4a9BzRr21nQT23GI2e4RlJ2Rkum9lazBNaVaQBYIUgLVVFMSfxbEt2GGBv82UKbQTbk6KHrrKE8ABYmkE81lgE+8zlnh6lxlaEQ9if6/KvtwP97g0md3hxc9b2MvGnQLEX9jjHJ/B9bHW7jJzqWRQAnCQZzenbyTht5lNK480Q9qGTu0h8FNteapzos/JnQ3B8taGQI5fpxosRLyhX3wzdQrmaAiBnILgYV2sPWZT3th0M6gsLDi4ao40=
-- > 123.100.200.3 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBJKYO35BsIkFjiAXACgkWzTC+tA2sH5RSqoYoGq8Lv+
-- > 123.100.200.3 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBKR0UH9ZSmUyYUJfE/4mUT4SLZ9wskvsCXkVL8QNIprmFt7Zz7eRerQVyqoOm4/Zhu2OWlleqfIWOmuyPDGkImo=
getSshHostKeys ::
  -- | IP Address or hostname.  Example: @\"123.100.200.3\"@
  Text ->
  IO Text
getSshHostKeys = undefined

-- | Return the fingerprints for a set of raw host keys.
--
-- This effectively just runs @ssh-keygen -l@ on a set of raw host keys.
--
-- See the docs for 'Fingerprint' for what this function outputs.
fingerprintsFromHostKeys ::
  -- | A newline-separated file of SSH host keys.  See the output of
  -- 'getSshHostKeys' for what this should look like.
  Text ->
  IO Text
fingerprintsFromHostKeys = undefined

-- | Return 'True' if two sets of fingerprints match.
doFingerprintsMatch :: NonEmpty Fingerprint -> NonEmpty Fingerprint -> Bool
doFingerprintsMatch = undefined

-- | Remove old, out-of-date host keys from the user's @~/.ssh/known_hosts@ file.
--
-- This effectively just runs @ssh-keygen -R@ on the passed-in IP address.
removeOldHostKeysFromKnownHosts ::
  -- | IP Address or hostname.  Example: @\"123.100.200.3\"@.
  Text ->
  IO ()
removeOldHostKeysFromKnownHosts = undefined

-- | Add a set of new SSH host keys to the @~/.ssh/known_hosts@ file.
--
-- This effectively just appends the passed-in host keys to the file.
addNewHostKeysToKnownHosts ::
  -- | A set of SSH host keys.  See the output of 'getSshHostKeys' for
  -- what this should look like.
  Text ->
  IO ()
addNewHostKeysToKnownHosts = undefined

-- $setup
--
-- >>> import Text.Parsec (parseTest)
