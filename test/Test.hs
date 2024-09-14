
module Main where

import Test.Cloudy (cloudyTests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain cloudyTests
