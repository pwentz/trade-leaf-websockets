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
    context "#parseNewChat" $
      it "can parse the format for a new chat" $ do
        parseNewChat "^^^ 12 ^^^" `shouldBe` Nothing
        parseNewChat "#(( 12 ))#" `shouldBe` Nothing
        parseNewChat "#{{ 12 }}#" `shouldBe` Nothing
        parseNewChat "^{{ 12 }}^" `shouldBe` Nothing
        parseNewChat "^{{12 - 15}}^" `shouldBe` Nothing
        parseNewChat "^{{ 12 - hi }}^" `shouldBe` Nothing
        parseNewChat "^{{ hi - 15 }}^" `shouldBe` Nothing
        parseNewChat "^{{ 12-15-17-5 }}^" `shouldBe` Nothing
        parseNewChat "^{{ 12-15 }}^" `shouldBe` Just (NewChat { chatId = 12, recipientId = 15 })
        parseNewChat "^{{ 12 - 15 }}^" `shouldBe` Just (NewChat { chatId = 12, recipientId = 15 })
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
