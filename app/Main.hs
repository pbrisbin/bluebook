module Main
    ( main
    ) where

import Bluebook.Prelude

import Bluebook.Convert
import Bluebook.HTML as HTML
import Bluebook.Listing
import Bluebook.ManPage.Section
import Bluebook.Settings
import qualified Data.Text.IO as T
import Network.HTTP.Types (Status, status200, status404, status405, status500)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.FilePath (joinPath)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html

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

            ps -> do
                let fileName = toManPageFileName ps
                result <- tryManPage2Html settings fileName

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
                            Html.code $ Html.toHtml fileName
                            " is not present on this system."

        m -> respond $ html status405 "Unsupported Method" $ do
            Html.header $ Html.h1 "Unsupported Method"
            Html.p $ do
                "This server does not respond to the "
                Html.code $ Html.toHtml $ show @Text m
                " method"

buildQuery :: [(ByteString, Maybe ByteString)] -> Maybe Query
buildQuery = foldMap $ uncurry toQuery
  where
    toQuery :: ByteString -> Maybe ByteString -> Maybe Query
    toQuery "q" (Just bs) = Just $ QueryByName $ decodeUtf8 bs
    toQuery _ _ = Nothing

toManPageFileName :: [Text] -> FilePath
toManPageFileName = dropSuffix ".html" . joinPath . map unpack

html :: Status -> Text -> Html -> Response
html s title =
    responseLBS s [("Content-Type", "text/html")]
        . Blaze.renderHtml
        . HTML.layout title
