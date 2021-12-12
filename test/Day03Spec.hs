module Day03Spec (spec) where

import Day03
import Test.Hspec

spec :: Spec
spec = 
    let testInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"
    in do
        describe "MCB" $ do
            context "on all zeroes" $ do
              it "returns zero" $ do
                mcb "00000" `shouldBe` 0

            context "on all ones" $ do
              it "returns one" $ do
                mcb "11111" `shouldBe` 1

            context "on odd number of bits, mostly ones" $ do
              it "returns one" $ do
                mcb "11001" `shouldBe` 1

            context "on even number of bits, same number of 1s and 0s" $ do
              it "returns zero" $ do
                mcb "110010" `shouldBe` 0

        describe "partOne" $ do
            context "on test input" $ do
                it "returns correct output" $ do
                  partOne testInput `shouldBe` 198

        describe "partTwo" $ do
            context "on test input" $ do
                it "returns correct output" $ do
                  partTwo testInput `shouldBe` Just 1
