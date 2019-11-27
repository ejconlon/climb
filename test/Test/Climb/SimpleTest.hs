module Test.Climb.SimpleTest where

import Climb.Prelude
import Test.Tasty
import Test.Tasty.HUnit

test_something :: TestTree
test_something = testCase "something" $ do
    let actual = (1 + 1) :: Int
        expected = 2 :: Int
    actual @?= expected
