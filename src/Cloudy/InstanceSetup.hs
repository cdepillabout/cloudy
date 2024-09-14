{-# LANGUAGE TemplateHaskell #-}


module Cloudy.InstanceSetup where

import Cloudy.InstanceSetup.Types
import Data.FileEmbed (embedDir)
import System.FilePath (takeBaseName)

embedBuiltInInstanceSetups :: [InstanceSetup]
embedBuiltInInstanceSetups =
  $(do
      rawFiles <- embedDir "instance-setup/"
      for rawFiles $ \(fp, rawData) -> do
        let name = takeBaseName fp
        eitherInstanceSetupData <- decodeEither' rawData
        case eitherInstanceSetupData of
          Left err -> fail $ "Failed to decode instance-setup data in " <> fp <> " as InstanceSetupData: " <> err
          Right instanceSetupData ->
            pure $ InstanceSetup { name, instanceSetupData }
    )


rawBuiltInInstanceSetups :: [InstanceSetup]
rawBuiltInInstanceSetups = undefined
