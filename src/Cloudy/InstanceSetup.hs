
module Cloudy.InstanceSetup where

import Data.Aeson (FromJSON(..), ToJSON (..), object, (.=), FromJSON, withObject, (.:))
import Data.Text (Text)


data InstanceSetup = InstanceSetup
  { shortDescription :: Text
  , cloudInitUserData :: Text
  }

instance ToJSON InstanceSetup where
  toJSON InstanceSetup{shortDescription, cloudInitUserData} =
    object
      [ "short-description" .= shortDescription
      , "cloud-init-user-data" .= cloudInitUserData
      ]

instance FromJSON InstanceSetup where
  parseJSON = withObject "InstanceSetup" $ \o -> do
    shortDescription <- o .: "short-description"
    cloudInitUserData <- o .: "cloud-init-user-data"
    pure InstanceSetup { shortDescription, cloudInitUserData }
