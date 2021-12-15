module Day01Spec (spec) where

import Day01
import Test.Hspec

spec :: Spec
spec =
  let testInput = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"
   in do
        describe "partOne" $ do
          context "on test input" $ do
            it "returns expected result" $ do
              partOne testInput `shouldBe` Just 7

        describe "partTwo" $ do
          context "on test input" $ do
            it "returns expected result" $ do
              partTwo testInput `shouldBe` Just 5
