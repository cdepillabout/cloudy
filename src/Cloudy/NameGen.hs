
module Cloudy.NameGen where

import Data.Text (Text)
import System.Random (randomRIO)

names :: [Text]
names =
  [ "angle", "ant", "apple", "arch", "bell", "berry", "bird", "blade", "board"
  , "boat", "bone", "book", "boot", "bottle", "box", "boy", "brain", "brake"
  , "branch", "brick", "bridge", "brush", "bucket", "bulb", "button", "cake"
  , "camera", "card", "cart", "carriage", "cat", "chain", "cheese", "drain"
  , "drawer", "dress", "drop", "ear", "egg", "engine", "eye", "face", "farm"
  , "feather", "finger", "fish", "flag", "floor", "fly", "foot", "fork"
  , "fowl", "frame", "garden", "girl", "glove", "goat", "gun", "hair"
  , "hammer", "hand", "hat", "head", "heart", "hook", "lock", "map", "match"
  , "monkey", "moon", "mouth", "muscle", "nail", "neck", "plane", "plate"
  , "plough", "pocket", "pot", "potato", "shoe", "skin", "sun", "table"
  , "tail", "thread", "throat", "thumb", "ticket", "toe", "tongue", "tooth"
  , "town", "train", "window", "wing", "wire", "worm"
  ]

randomName :: IO Text
randomName = do
  nameIdx <- randomRIO (0, length names - 1)
  pure $ names !! nameIdx

instanceNameGen :: IO Text
instanceNameGen = do
  name1 <- randomName
  name2 <- randomName
  pure $ "cloudy-" <> name1 <> "-" <> name2
