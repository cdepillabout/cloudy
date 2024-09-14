
module Cloudy.InstanceSetup.Types where

import Data.Aeson (FromJSON(..), ToJSON (..), object, (.=), FromJSON, withObject, (.:))
import Data.Text (Text)

data InstanceSetup = InstanceSetup
  { name :: Text
  , instanceSetupData :: InstanceSetupData
  }

data InstanceSetupData = InstanceSetupData
  { shortDescription :: Text
  , cloudInitUserData :: Text
  }

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
