module Foreign.CRCSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False
