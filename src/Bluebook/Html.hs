module Bluebook.Html
    ( renderHtmlIndex
    , renderHtmlManPage
    , renderHtml
    , notFoundHtml
    , errorHtml
    ) where

import Bluebook.Prelude

import Bluebook.Error
import Bluebook.ManPage (ManPage)
import qualified Bluebook.ManPage as ManPage
import qualified Data.Text as T
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (docTypeHtml, toHtml, toValue, (!))
import qualified Text.Blaze.Html5.Attributes as Html
    (charset, class_, href, rel)

renderHtmlIndex :: Maybe Text -> [ManPage] -> ByteString
renderHtmlIndex mTitle pages = renderHtml mTitle $ do
    Html.header $ do
        Html.h1 $ toHtml $ maybe "man/" (\s -> "man/" <> s <> "/") mTitle
    Html.section $ Html.ul $ for_ (sortOn ManPage.ref pages) $ \page ->
        Html.li
            $ Html.a
            ! Html.href (toValue $ ManPage.url page)
            $ toHtml
            $ ManPage.ref page

renderHtmlManPage :: ManPage -> Html -> ByteString
renderHtmlManPage page html = renderHtml (Just title) $ do
    Html.section ! Html.class_ "man-page" $ do
        Html.header $ Html.h1 $ Html.toHtml title
        html
        Html.footer $ Html.ul $ do
            Html.li $ toHtml $ sectionName $ ManPage.section page
            Html.li $ toHtml title
  where
    title = T.toUpper $ ManPage.ref page

    sectionName :: Int -> Text
    sectionName = \case
        1 -> "User Commands"
        2 -> "System Calls"
        3 -> "Library Calls"
        4 -> "Devices"
        5 -> "Files"
        6 -> "Games"
        7 -> "Overviews, Conventions, and Miscellaneous"
        8 -> "System Management Commands"
        _ -> "N/A"

renderHtml :: Maybe Text -> Html -> ByteString
renderHtml mTitle body = toStrict $ Blaze.renderHtml $ docTypeHtml $ do
    Html.head $ do
        Html.meta ! Html.charset "UTF-8"
        Html.link ! Html.rel "stylesheet" ! Html.href "/css/main.css"
        Html.title $ toHtml $ maybe "Bluebook" ("Bluebook - " <>) mTitle
    Html.body $ do
        Html.nav $ Html.ul $ do
            Html.li $ Html.a ! Html.href "/" $ "Home"
            for_ [1 .. 8] $ \section -> do
                let name = "man" <> show @Text @Int section
                    link = toValue $ "/man" <> show @Text section <> "/"
                Html.li $ Html.a ! Html.href link $ toHtml name

        body

        Html.footer $ do
            "Built with "
            Html.strong
                $ Html.a
                ! Html.href (toValue $ ManPage.url ManPage.bluebook)
                $ toHtml
                $ ManPage.ref ManPage.bluebook
            " — © 2023 Patrick Brisbin"

notFoundHtml :: ByteString
notFoundHtml = renderHtml (Just "Not Found") $ Html.p "Page not found"

errorHtml :: Error -> Html
errorHtml e = do
    Html.p "There was an error rendering this man-page:"
    Html.pre $ Html.code $ toHtml $ displayException e
