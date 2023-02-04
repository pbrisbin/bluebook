{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Bluebook.Prelude

import Bluebook.Convert
import Data.FileEmbed
import qualified Data.Text.IO as T
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (docTypeHtml, toHtml, (!))
import qualified Text.Blaze.Html5.Attributes as Html (charset)

main :: IO ()
main = do
    man <- T.getContents
    (html, details) <- man2Html Nothing man
    T.putStrLn $ decodeUtf8 $ renderHtmlManPage (title details) html

renderHtmlManPage :: Text -> Html -> ByteString
renderHtmlManPage title html = toStrict $ Blaze.renderHtml $ docTypeHtml $ do
    Html.head $ do
        Html.meta ! Html.charset "UTF-8"
        Html.style $ toHtml $ decodeUtf8 @Text css
        Html.title $ toHtml title

    Html.body html

css :: ByteString
css = $(embedFile "bluebook.css")
