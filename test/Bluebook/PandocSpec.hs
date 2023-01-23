module Bluebook.PandocSpec
    ( spec
    ) where

import           Bluebook.Prelude

import           Bluebook.Pandoc
import           Test.Hspec

spec :: Spec
spec = do
    describe "addHeaderLinks" $ do
        it "adds identifiers and links them" $ do
            let doc = Header 1 nullAttr [Str "NAME"]

            addHeaderLinks doc `shouldBe` Header
                1
                ("name", [], [])
                [Link nullAttr [Str "NAME"] ("#name", "Link to this section")]

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

    describe "convertManPageRefs" $ do
        let linked ref page = Link nullAttr [Strong [Str ref]] (page, ref)

        it "handles bare Str references" $ do
            let input = Para [Str "See ", Str "foo(1)", Str " for details"]

            convertManPageRefs input
                `shouldBe` Para
                               [ Str "See "
                               , linked "foo(1)" "/man1/foo.1.html"
                               , Str " for details"
                               ]

        it "handles bolded references" $ do
            let
                input =
                    Para [Str "See ", Strong [Str "foo(1)"], Str " for details"]

            convertManPageRefs input
                `shouldBe` Para
                               [ Str "See "
                               , linked "foo(1)" "/man1/foo.1.html"
                               , Str " for details"
                               ]
