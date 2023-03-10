module Main (main) where

import Data.Bool.Kill
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Kill-Bool" $ do
    describe "mkIs" $ do
      let isPresent :: TT
          isPresent = mkIs $ Proxy @"present"
          isMissing :: TT
          isMissing = mkIs $ Proxy @"missing"
      describe "isPresent" $ do
        it "is 'present' should be True" $ is (Proxy @"present") isPresent `shouldBe` True
        it "isNot 'present' should be False" $ isNot (Proxy @"present") isPresent `shouldBe` False
        it "is 'missing' should be False" $ is (Proxy @"missing") isPresent `shouldBe` False
        it "isNot 'missing' should be True" $ isNot (Proxy @"missing") isPresent `shouldBe` True
      describe "isMissing" $ do
        it "is 'missing' should be True" $ is (Proxy @"missing") isMissing `shouldBe` True
        it "isNot 'missing' should be False" $ isNot (Proxy @"missing") isMissing `shouldBe` False
        it "is 'present' should be False" $ is (Proxy @"present") isMissing `shouldBe` False
        it "isNot 'present' should be True" $ isNot (Proxy @"present") isMissing `shouldBe` True
    describe "mkTBool" $ do
      let isPresent :: TT
          isPresent = mkTBool True
          isMissing :: TT
          isMissing = mkTBool False
      describe "isPresent" $ do
        it "is 'present' should be True" $ is (Proxy @"present") isPresent `shouldBe` True
        it "isNot 'present' should be False" $ isNot (Proxy @"present") isPresent `shouldBe` False
        it "is 'missing' should be False" $ is (Proxy @"missing") isPresent `shouldBe` False
        it "isNot 'missing' should be True" $ isNot (Proxy @"missing") isPresent `shouldBe` True
      describe "isMissing" $ do
        it "is 'missing' should be True" $ is (Proxy @"missing") isMissing `shouldBe` True
        it "isNot 'missing' should be False" $ isNot (Proxy @"missing") isMissing `shouldBe` False
        it "is 'present' should be False" $ is (Proxy @"present") isMissing `shouldBe` False
        it "isNot 'present' should be True" $ isNot (Proxy @"present") isMissing `shouldBe` True

type TT = TBool "missing" "present"
