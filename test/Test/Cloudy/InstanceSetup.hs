
module Test.Cloudy.InstanceSetup where

import Cloudy.InstanceSetup (builtInInstanceSetups)
import Control.Exception (evaluate)
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

instanceSetupTests :: TestTree
instanceSetupTests =
  testGroup
    "InstanceSetup"
    [ builtInInstanceSetupsNoException
    ]

builtInInstanceSetupsNoException :: TestTree
builtInInstanceSetupsNoException =
  testCase "builtInInstanceSetups should never throw an exception" $
    void $ evaluate builtInInstanceSetups
