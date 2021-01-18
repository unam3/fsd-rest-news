{-# LANGUAGE OverloadedStrings  #-}

module RestNewsSpec where

import System.Process (readProcess)
import RestNews (runWarpWithLogger)
import Test.Hspec

spec :: Spec
spec = do
    --runWarpWithLogger
    describe "get" .
        it "returns Int" $ (read ("5" :: String) :: Int) `shouldBe` (5 :: Int)

    describe "date" .
        it "returns date" $ readProcess "date" [] [] >>= (`shouldBe` ("5" :: String))
