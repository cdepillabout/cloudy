{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Cloudy.InstanceSetup where

import Cloudy.InstanceSetup.Types ( InstanceSetup(..) )
import Cloudy.Path (getCloudyInstanceSetupsDir)
import Control.DeepSeq (force)
import Control.FromSum (fromEither)
import Data.Bifunctor (Bifunctor(first))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.FileEmbed (embedDir)
import Data.Functor ((<&>))
import Data.List (sort, find)
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Yaml (decodeEither', ParseException (OtherParseException))
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Control.Exception (SomeException(SomeException))

rawBuiltInInstanceSetups :: [(FilePath, ByteString)]
rawBuiltInInstanceSetups = $(embedDir "instance-setups/")

builtInInstanceSetups :: [InstanceSetup]
builtInInstanceSetups =
  -- force is used here because we want to make sure any YAML decoding errors
  -- are surfaced the very first time builtInInstanceSetups is used.  We should
  -- almost never see any errors, since we control the raw instance-setup files
  -- that are used here.
  --
  -- We also have tests that should catch problems with decoding these
  -- instance-setup files.
  force $
    sort rawBuiltInInstanceSetups <&> \(fp, rawData) ->
      fromEither
        (\err ->
            error $
              "Failed to decode instance-setup data in " <> fp <>
              " as InstanceSetupData: " <> show err
        )
        (parseInstanceSetup fp rawData)

parseInstanceSetup :: FilePath -> ByteString -> Either ParseException InstanceSetup
parseInstanceSetup fp rawData = do
  let name = pack $ takeBaseName fp
  rawDataText <- first (OtherParseException . SomeException) $ decodeUtf8' rawData
  instanceSetupData <- decodeEither' rawData
  pure $ InstanceSetup { name, instanceSetupData, rawInstanceSetupData = rawDataText }


getUserInstanceSetups :: IO [InstanceSetup]
getUserInstanceSetups = do
  instanceSetupsDir <- getCloudyInstanceSetupsDir
  files <- fmap (instanceSetupsDir </>) <$> listDirectory instanceSetupsDir
  let yamlFiles = filter isYamlExt files
  traverse yamlFileToInstanceSetup yamlFiles
  where
    isYamlExt :: FilePath -> Bool
    isYamlExt fp = takeExtension fp == ".yaml" || takeExtension fp == ".yml"

yamlFileToInstanceSetup :: FilePath -> IO InstanceSetup
yamlFileToInstanceSetup rawInstanceSetupFp = do
  rawInstanceSetupData <- ByteString.readFile rawInstanceSetupFp
  case parseInstanceSetup rawInstanceSetupFp rawInstanceSetupData of
    Left err -> error $ "Failed to decode " <> rawInstanceSetupFp <> " as instance-setup: " <> show err
    Right instanceSetup -> pure instanceSetup

-- | Find an 'InstanceSetup' from within the built-in and user-defined instance
-- setups.
findInstanceSetup ::
  -- | Name of the 'InstanceSetup' to look for.
  Text ->
  IO (Maybe InstanceSetup)
findInstanceSetup nameToFind = do
  userInstanceSetups <- getUserInstanceSetups
  let allInstanceSetups = userInstanceSetups <> builtInInstanceSetups
      maybeInstanceSetup :: Maybe InstanceSetup = find (\instSetup -> instSetup.name == nameToFind) allInstanceSetups
  pure maybeInstanceSetup
