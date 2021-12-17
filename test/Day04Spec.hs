module Day04Spec (spec) where

import Data.Array
import Day04
import Test.Hspec

spec :: Spec
spec =
  let testInput =
        "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
        \\n\
        \22 13 17 11  0\n\
        \ 8  2 23  4 24\n\
        \21  9 14 16  7\n\
        \ 6 10  3 18  5\n\
        \ 1 12 20 15 19\n\
        \\n\
        \3 15  0  2 22\n\
        \9 18 13 17  5\n\
        \19  8  7 25 23\n\
        \20 11 10 24  4\n\
        \14 21 16 12  6\n\
        \\n\
        \14 21 17 24  4\n\
        \10 16 15  9 19\n\
        \18  8 23 26 20\n\
        \22 11 13  6  5\n\
        \ 2  0 12  3  7"
   in do
        describe "parseNumBoard" $ do
          context "on simple input" $
            it "returns parsed board" $
              parseNumBoard "1 2 3 4 5\n6 7 8 9 10\n11 12 13 14 15\n16 17 18 19 20\n21 22 23 24 25\n"
                `shouldBe` Just (listArray ((0, 0), (4, 4)) [1 .. 25])

          context "on invalid input" $
            it "returns nothing" $
              parseNumBoard "1 2 3 a 5\n6 7 8 9 10\n11 12 b 14 15\n16 17 18 c 20\n21 22 23 24 25\n" `shouldBe` Nothing

        describe "mark" $
          context "on single marked number" $
            it "returns 1 in that location" $
              let nums = listArray boardBounds [1 .. 25]
               in do
                    hits (mark 1 (mkBingo nums)) `shouldBe` listArray boardBounds (True : repeat False)
                    hits (mark 3 (mkBingo nums)) `shouldBe` listArray boardBounds (False : False : True : repeat False)
                    hits (mark 25 (mkBingo nums)) `shouldBe` listArray boardBounds (replicate 24 False ++ [True])

        describe "hasBingo" $ do
          context "on non-bingo boards" $
            it "returns false" $ do
              hasBingo (listArray boardBounds (True : repeat False)) `shouldBe` False
              hasBingo (listArray boardBounds (False : False : True : repeat False)) `shouldBe` False
              hasBingo (listArray boardBounds (True : True : True : repeat False)) `shouldBe` False

          context "on row-bingo boards" $
            it "returns true" $ do
              hasBingo (listArray boardBounds (replicate 5 True ++ repeat False)) `shouldBe` True
              hasBingo (listArray boardBounds (replicate 10 False ++ replicate 5 True ++ repeat False)) `shouldBe` True
              hasBingo (listArray boardBounds (replicate 20 False ++ replicate 5 True)) `shouldBe` True

          context "on column-bingo boards" $
            it "returns true" $ do
              hasBingo (listArray boardBounds (cycle [False, True, False, False, False])) `shouldBe` True
              hasBingo (listArray boardBounds (cycle [False, False, True, True, False])) `shouldBe` True

        describe "partOne" $ do
          context "on test input" $
            it "returns expected result" $
              partOne testInput `shouldBe` Just 4512
