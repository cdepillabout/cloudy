
module Test.Cloudy where

import Test.Cloudy.InstanceSetup (instanceSetupTests)
import Test.Tasty (TestTree, testGroup)

cloudyTests :: TestTree
cloudyTests =
  testGroup
    "Cloudy"
    [ instanceSetupTests
    ]
