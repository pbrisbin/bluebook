module Bluebook.ManPageSpec
    ( spec
    ) where

import Bluebook.Prelude

import Bluebook.ManPage
import Test.Hspec

spec :: Spec
spec = do
    describe "newManPage" $ do
        it "accepts typical man-pages" $ do
            newManPage "/usr/share/man" "man1/foo.1" `shouldBe` Just ManPage
                { sourcePath = "/usr/share/man/man1/foo.1"
                , outputPath = "man1/foo.1.html"
                , section = 1
                , name = "foo"
                , url = "/man1/foo.1.html"
                , ref = "foo(1)"
                }

            newManPage "/usr/share/man" "man7/bar.7" `shouldBe` Just ManPage
                { sourcePath = "/usr/share/man/man7/bar.7"
                , outputPath = "man7/bar.7.html"
                , section = 7
                , name = "bar"
                , url = "/man7/bar.7.html"
                , ref = "bar(7)"
                }

        context "sourcePath" $ do
            it "captures gz extensions" $ do
                newManPage "/usr/share/man" "man1/foo.1.gz" `shouldBe` Just
                    ManPage
                        { sourcePath = "/usr/share/man/man1/foo.1.gz"
                        , outputPath = "man1/foo.1.html"
                        , section = 1
                        , name = "foo"
                        , url = "/man1/foo.1.html"
                        , ref = "foo(1)"
                        }

            it "captures the parent" $ do
                newManPage "/usr/local/share/man" "man1/foo.1.gz"
                    `shouldBe` Just ManPage
                                   { sourcePath =
                                       "/usr/local/share/man/man1/foo.1.gz"
                                   , outputPath = "man1/foo.1.html"
                                   , section = 1
                                   , name = "foo"
                                   , url = "/man1/foo.1.html"
                                   , ref = "foo(1)"
                                   }
