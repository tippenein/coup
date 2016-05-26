module CoupSpec (spec) where

import Coup

import Test.Hspec

spec :: Spec
spec =
  describe "main" $ do
    it "returns the unit" $
      main `shouldReturn` ()
