module Day02Spec (spec) where

import Day02
import Test.Hspec

spec :: Spec
spec =
  let testInput = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
   in do
        describe "partOne" $ do
          context "on test input" $ do
            it "returns correct output" $ do
              partOne testInput `shouldBe` Just 150

        describe "partTwo" $ do
          context "on test input" $ do
            it "returns correct output" $ do
              partTwo testInput `shouldBe` Just 900
