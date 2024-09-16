{-# LANGUAGE TemplateHaskell #-}

module Cloudy.InstanceSetup where

import Cloudy.InstanceSetup.Types ( InstanceSetup(..) )
import Cloudy.Path (getCloudyInstanceSetupsDir)
import Control.DeepSeq (force)
import Control.FromSum (fromEither)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.FileEmbed (embedDir)
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Text (pack)
import Data.Yaml (decodeEither', ParseException)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

rawBuiltInInstanceSetups :: [(FilePath, ByteString)]
rawBuiltInInstanceSetups = $(embedDir "instance-setups/")

builtInInstanceSetups :: [InstanceSetup]
builtInInstanceSetups =
  -- We want to make sure any YAML decoding errors are surfaced the very first
  -- time builtInInstanceSetups is used.  We should almost never see any
  -- errors, since we control the raw instance-setup files that are used here.
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
  instanceSetupData <- decodeEither' rawData
  pure $ InstanceSetup { name, instanceSetupData, rawInstanceSetupData = rawData }


getUserInstanceSetups :: IO [InstanceSetup]
getUserInstanceSetups = do
  instanceSetupsDir <- getCloudyInstanceSetupsDir
  files <- fmap (instanceSetupsDir </>) <$> listDirectory instanceSetupsDir
  print files
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
