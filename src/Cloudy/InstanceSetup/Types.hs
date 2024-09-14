{-# LANGUAGE DeriveAnyClass #-}

module Cloudy.InstanceSetup.Types where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON (..), object, (.=), FromJSON, withObject, (.:))
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

data InstanceSetup = InstanceSetup
  { name :: Text
  , instanceSetupData :: InstanceSetupData
  , rawInstanceSetupData :: ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

data InstanceSetupData = InstanceSetupData
  { shortDescription :: Text
  , cloudInitUserData :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance ToJSON InstanceSetupData where
  toJSON InstanceSetupData{shortDescription, cloudInitUserData} =
    object
      [ "short-description" .= shortDescription
      , "cloud-init-user-data" .= cloudInitUserData
      ]

instance FromJSON InstanceSetupData where
  parseJSON = withObject "InstanceSetupData" $ \o -> do
    shortDescription <- o .: "short-description"
    cloudInitUserData <- o .: "cloud-init-user-data"
    pure InstanceSetupData { shortDescription, cloudInitUserData }
