module MessageParserSpec where

import Test.Hspec
import MessageParser

spec :: Spec
spec =
  describe "MessageParser" $ do
    context "#parseNewUser" $
      it "can parse the format for a new user" $ do
        parseNewUser "--- 12 ---" `shouldBe` Nothing
        parseNewUser "#(( 12" `shouldBe` Nothing
        parseNewUser "#(( 12 ))#" `shouldBe` Nothing
        parseNewUser "#{{12}}#" `shouldBe` Nothing
        parseNewUser "#{{ 12 }}#" `shouldBe` Just 12
    context "#parseMessage" $
      it "can parse the format for a new message" $ do
        parseMessage "--- 12 ---: some msg" `shouldBe` Nothing
        parseMessage "#(( 12: some msg" `shouldBe` Nothing
        parseMessage "#(( 12 ))#: some msg" `shouldBe` Nothing
        parseMessage "#{{12}}#: some msg" `shouldBe` Nothing
        parseMessage "#{{ 12 }}# - some msg" `shouldBe` Nothing
        parseMessage "#{{ 12 }}#: " `shouldBe` Nothing
        parseMessage "#{{ 12 }}#:some msg" `shouldBe` Nothing
        parseMessage "#{{ 12 }}#: some msg" `shouldBe` Just (12, "some msg")
