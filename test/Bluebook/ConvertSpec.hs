module Bluebook.ConvertSpec
    ( spec
    ) where

import Bluebook.Prelude

import Bluebook.Convert
import Test.Hspec
import Text.Pandoc.Definition (Block(..), Inline(..), nullAttr)

spec :: Spec
spec = do
    describe "addHeaderLinks" $ do
        it "adds identifiers and links them" $ do
            let doc = Header 1 nullAttr [Str "NAME"]

            addHeaderLinks doc `shouldBe` Header
                1
                ("name", [], [])
                [Link nullAttr [Str "NAME"] ("#name", "")]

    describe "reduceHeaderLevels" $ do
        it "reduces all headers by 1" $ do
            let h1 = Header 1 nullAttr []
                h2 = Header 2 nullAttr []
                h3 = Header 3 nullAttr []
                h4 = Header 4 nullAttr []
                h5 = Header 5 nullAttr []
                h6 = Header 6 nullAttr []

            reduceHeaderLevels h1 `shouldBe` h2
            reduceHeaderLevels h2 `shouldBe` h3
            reduceHeaderLevels h3 `shouldBe` h4
            reduceHeaderLevels h4 `shouldBe` h5
            reduceHeaderLevels h5 `shouldBe` h6
            reduceHeaderLevels h6 `shouldBe` h6 -- bottom

    describe "linkBareUrls" $ do
        it "handles bracketed URLs" $ do
            let input = "For more details see <https://example.com>."
            linkBareUrls [Str input]
                `shouldBe` [ Str "For"
                           , Str "more"
                           , Str "details"
                           , Str "see"
                           , Link
                               ("", [], [])
                               [Str "https://example.com"]
                               ("https://example.com", "")
                           , Str "."
                           ]
