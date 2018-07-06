{-# LANGUAGE OverloadedStrings #-}

module App.LibSpec
  ( spec
  ) where

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "App.OptionsSpec" $ do
  it "example test" $
    True `shouldBe` True
