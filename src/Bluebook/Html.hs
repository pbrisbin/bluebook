module Bluebook.Html
    ( renderHtmlIndex
    , renderHtml
    , notFoundHtml
    , errorHtml
    ) where

import Bluebook.Prelude

import Bluebook.Error
import Bluebook.ManPage (ManPage)
import qualified Bluebook.ManPage as ManPage
import Data.Version (showVersion)
import Paths_bluebook as Pkg
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (docTypeHtml, toHtml, toValue, (!))
import qualified Text.Blaze.Html5.Attributes as Html (charset, href, rel)

renderHtmlIndex :: Text -> Text -> [ManPage] -> ByteString
renderHtmlIndex root title pages = renderHtml root title $ do
    Html.header $ Html.h1 $ toHtml title
    Html.section $ Html.ul $ for_ (sortOn ManPage.ref pages) $ \page ->
        Html.li
            $ Html.a
            ! Html.href (toValue $ ManPage.url page)
            $ toHtml
            $ ManPage.ref page

renderHtml :: Text -> Text -> Html -> ByteString
renderHtml root title body = toStrict $ Blaze.renderHtml $ docTypeHtml $ do
    Html.head $ do
        Html.meta ! Html.charset "UTF-8"
        Html.link ! Html.rel "stylesheet" ! Html.href
            (toValue root <> "css/main.css")
        Html.title $ toHtml title
    Html.body $ do
        Html.nav $ Html.ul $ do
            Html.li $ Html.a ! Html.href (toValue root) $ "Home"
            for_ [1 .. 8] $ \section -> do
                let name = "man" <> show @Text @Int section
                    link = toValue $ root <> "man" <> show @Text section <> "/"
                Html.li $ Html.a ! Html.href link $ toHtml name

        body

        Html.footer $ do
            "Built with "
            Html.strong $ Html.a ! Html.href (toValue docsUrl) $ "bluebook(1)"
            toHtml $ " v" <> showVersion Pkg.version
            " — © 2023 Patrick Brisbin"

docsUrl :: Text
docsUrl = "https://pbrisbin.github.io/bluebook/man1/bluebook.1.html"

notFoundHtml :: Html
notFoundHtml = Html.p "Page not found"

errorHtml :: Error -> Html
errorHtml e = do
    Html.p "There was an error rendering this man-page:"
    Html.pre $ Html.code $ toHtml $ displayException e
