module Bluebook.Handler.Html
    ( defaultLayout

    -- * Errors
    , throwBadRequest
    , throwNotFound
    , throwServerError

    -- * Combinators
    , fromMaybeNotFound
    , fromEitherBadRequest
    , fromEitherServerError

    -- * Re-exports
    , Handler
    , Html
    , toHtml
    ) where

import Bluebook.Prelude
import qualified Text.Blaze.Html5.Attributes as Html hiding (style, title)

import qualified Bluebook.CSS as CSS
import Bluebook.ManPage.Section
import qualified Data.Text.IO as T
import Network.HTTP.Types (hContentType)
import Servant
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (docTypeHtml, toHtml, toValue, (!))

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

throwBadRequest :: Html -> Handler a
throwBadRequest body = throwError err400
    { errBody = Blaze.renderHtml $ defaultLayout "Bad Request" body
    , errHeaders = [(hContentType, "text/html")]
    }

throwNotFound :: Html -> Handler a
throwNotFound body = throwError err404
    { errBody = Blaze.renderHtml $ defaultLayout "Not Found" body
    , errHeaders = [(hContentType, "text/html")]
    }

throwServerError :: Html -> Handler a
throwServerError body = throwError err500
    { errBody = Blaze.renderHtml $ defaultLayout "Server Error" body
    , errHeaders = [(hContentType, "text/html")]
    }

fromMaybeNotFound :: Maybe a -> Handler a
fromMaybeNotFound = flip maybe pure $ throwNotFound $ do
    Html.header $ Html.h1 "Not Found"
    Html.p "This page does not exist."

fromEitherBadRequest :: (e -> Text) -> Either e a -> Handler a
fromEitherBadRequest toMessage = flip either pure $ \err -> do
    throwBadRequest $ do
        Html.header $ Html.h1 "Bad Request"
        Html.p $ toHtml $ toMessage err

fromEitherServerError :: Either Text a -> Handler a
fromEitherServerError = flip either pure $ \err -> do
    liftIO $ T.hPutStrLn stderr $ "[ERROR]: " <> err
    throwServerError $ do
        Html.header $ Html.h1 "Internal Server Error"
        Html.p "There was an error processing this manual"
