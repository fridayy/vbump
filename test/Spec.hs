import Test.Hspec
import Vbump

main :: IO ()
main = hspec $ do
    describe "Vbump.Test" $ do
        it "returns the expected version string" $ do
             bump "major" (Just ReleaseVersion { major = 1, minor = 0, patch = 0}) `shouldBe` "2.0.0"
             bump "minor" (Just ReleaseVersion { major = 1, minor = 0, patch = 0}) `shouldBe` "1.1.0"
             bump "patch" (Just ReleaseVersion { major = 1, minor = 0, patch = 0}) `shouldBe` "1.0.1"

        it "using nothing as current version returns the expected bumped version" $ do
            bump "major" Nothing `shouldBe` "1.0.0"
            bump "minor" Nothing `shouldBe` "0.2.0"
            bump "patch" Nothing `shouldBe` "0.1.1"