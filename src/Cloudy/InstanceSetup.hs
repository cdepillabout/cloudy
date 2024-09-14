{-# LANGUAGE TemplateHaskell #-}

module Cloudy.InstanceSetup where

import Cloudy.InstanceSetup.Types
import Control.DeepSeq (force)
import Control.FromSum (fromEither)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.Text (pack)
import Data.Yaml (decodeEither', ParseException)
import System.FilePath (takeBaseName)
import Data.Functor ((<&>))

rawBuiltInInstanceSetups :: [(FilePath, ByteString)]
rawBuiltInInstanceSetups = $(embedDir "instance-setup/")

builtInInstanceSetups :: [InstanceSetup]
builtInInstanceSetups =
  -- We want to make sure any YAML decoding errors are surfaced the very first
  -- time builtInInstanceSetups is used.  We should almost never see any
  -- errors, since we control the raw instance-setup files that are used here.
  --
  -- We also have tests that should catch problems with decoding these
  -- instance-setup files.
  force $
    rawBuiltInInstanceSetups <&> \(fp, rawData) ->
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
