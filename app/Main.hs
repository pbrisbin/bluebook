module Main
    ( main
    ) where

import Bluebook.Prelude

import Bluebook.Convert
import qualified Bluebook.CSS as CSS
import Bluebook.Listing
import Bluebook.ManPage.Section
import Bluebook.Settings
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Types (Status, status200, status404, status405, status500)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (docTypeHtml, toHtml, toValue, (!))
import qualified Text.Blaze.Html5.Attributes as Html hiding (style, title)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    settings@Settings {..} <- loadSettings
    putStrLn $ "Listening on port " <> show settingsPort
    run settingsPort $ app settings

app :: Settings -> Application
app settings req respond = do
    case requestMethod req of
        "GET" -> case pathInfo req of
            [] -> do
                let q = fromMaybe QueryAll $ buildQuery $ queryString req
                listing <- buildListing settings q
                respond $ html status200 "Bluebook" $ listingToHtml listing

            [p] | Right section <- sectionFromPath $ unpack p -> do
                let s = QueryBySection section
                    q = maybe s (s <>) $ buildQuery $ queryString req
                listing <- buildListing settings q
                respond $ html status200 "Bluebook" $ listingToHtml listing

            ps | Right name <- toManPageFileName ps -> do
                result <- tryManPage2Html settings name

                case result of
                    Just (Left err) -> do
                        T.hPutStrLn stderr $ "[ERROR] " <> manPageErrorText err
                        respond $ html status500 "Internal Server Error" $ do
                            Html.header $ Html.h1 "Internal Server Error"
                            Html.p "There was an error processing this manual"
                    Just (Right page) -> respond $ html
                        status200
                        (manPageTitle page)
                        (manPageBody page)
                    Nothing -> respond $ html status404 "Not found" $ do
                        Html.header $ Html.h1 "Not Found"
                        Html.p $ do
                            "The manual "
                            Html.code $ toHtml name
                            " is not present on this system."

            -- health-check
            ["ping"] -> do
                respond $ html status200 "Hello world" $ do
                    Html.header $ Html.h1 "Hello world"
                    Html.p "This service is up."

            _ -> do
                respond $ html status404 "Not found" $ do
                    Html.header $ Html.h1 "Not Found"
                    Html.p "Page not found."

        m -> respond $ html status405 "Unsupported Method" $ do
            Html.header $ Html.h1 "Unsupported Method"
            Html.p $ do
                "This server does not respond to the "
                Html.code $ toHtml $ show @Text m
                " method"

buildQuery :: [(ByteString, Maybe ByteString)] -> Maybe Query
buildQuery = foldMap $ uncurry toQuery
  where
    toQuery :: ByteString -> Maybe ByteString -> Maybe Query
    toQuery "q" (Just bs) = Just $ QueryByName $ decodeUtf8 bs
    toQuery _ _ = Nothing

-- | @man[1-8]/base.html@ -> @manX/base@
toManPageFileName :: [Text] -> Either String FilePath
toManPageFileName = \case
    [man, page] -> do
        base <- note "Missing .html" $ T.stripSuffix ".html" page
        section <- sectionFromPath $ unpack man
        pure $ sectionPath section </> unpack base

    _ -> Left "Too many components"

html :: Status -> Text -> Html -> Response
html s title =
    responseLBS s [("Content-Type", "text/html")]
        . Blaze.renderHtml
        . defaultLayout title

defaultLayout :: Text -> Html -> Html
defaultLayout title body = docTypeHtml $ do
    Html.head $ do
        Html.meta ! Html.charset "UTF-8"
        Html.title $ toHtml title
        Html.style $ toHtml CSS.styles
    Html.body $ do
        Html.nav $ do
            Html.ul $ do
                Html.li $ (Html.a ! Html.href "/") "Home"
                for_ [minBound .. maxBound] $ \section -> do
                    let path = sectionPath section
                        link = toValue $ "/" <> path
                    Html.li $ (Html.a ! Html.href link) $ toHtml path

        body

        Html.footer $ do
            "Built with "
            (Html.a ! Html.href "https://github.com/pbrisbin/bluebook")
                "Bluebook"
            " © 2023 Patrick Brisbin"
