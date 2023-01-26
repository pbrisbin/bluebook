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
import Bluebook.RenderLink
import Network.HTTP.Types (hContentType)
import Servant
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (docTypeHtml, toHtml, toValue, (!))

defaultLayout
    :: (MonadReader env m, HasRenderLink env) => Text -> Html -> m Html
defaultLayout title body = do
    rl <- view renderLinkL

    pure $ docTypeHtml $ do
        Html.head $ do
            Html.meta ! Html.charset "UTF-8"
            Html.title $ toHtml title
            Html.style $ toHtml CSS.styles
        Html.body $ do
            Html.nav $ do
                Html.ul $ do
                    Html.li
                        $ (Html.a ! Html.href (toValue $ renderLinkToRoot rl))
                              "Home"
                    for_ [minBound .. maxBound] $ \section -> do
                        let name = sectionPath section
                            path = "/" <> pack name
                            link = toValue $ renderLinkToDirectory rl path
                        Html.li $ (Html.a ! Html.href link) $ toHtml name

            body

            Html.footer $ do
                "Built with "
                (Html.a ! Html.href "https://github.com/pbrisbin/bluebook")
                    "Bluebook"
                " © 2023 Patrick Brisbin"

throwBadRequest
    :: (MonadError ServerError m, MonadReader env m, HasRenderLink env)
    => Html
    -> m a
throwBadRequest body = do
    html <- defaultLayout "Server Error" body
    throwError err400
        { errBody = Blaze.renderHtml html
        , errHeaders = [(hContentType, "text/html")]
        }

throwNotFound
    :: (MonadError ServerError m, MonadReader env m, HasRenderLink env)
    => Html
    -> m a
throwNotFound body = do
    html <- defaultLayout "Server Error" body
    throwError err404
        { errBody = Blaze.renderHtml html
        , errHeaders = [(hContentType, "text/html")]
        }

throwServerError
    :: (MonadError ServerError m, MonadReader env m, HasRenderLink env)
    => Html
    -> m a
throwServerError body = do
    html <- defaultLayout "Server Error" body
    throwError err500
        { errBody = Blaze.renderHtml html
        , errHeaders = [(hContentType, "text/html")]
        }

fromMaybeNotFound
    :: (MonadError ServerError m, MonadReader env m, HasRenderLink env)
    => Maybe a
    -> m a
fromMaybeNotFound = flip maybe pure $ throwNotFound $ do
    Html.header $ Html.h1 "Not Found"
    Html.p "This page does not exist."

fromEitherBadRequest
    :: (MonadError ServerError m, MonadReader env m, HasRenderLink env)
    => (e -> Text)
    -> Either e a
    -> m a
fromEitherBadRequest toMessage = flip either pure $ \err -> do
    throwBadRequest $ do
        Html.header $ Html.h1 "Bad Request"
        Html.p $ toHtml $ toMessage err

fromEitherServerError
    :: ( MonadLogger m
       , MonadError ServerError m
       , MonadReader env m
       , HasRenderLink env
       )
    => Either Text a
    -> m a
fromEitherServerError = flip either pure $ \err -> do
    logError $ "Internal Server Error" :# ["error" .= err]
    throwServerError $ do
        Html.header $ Html.h1 "Internal Server Error"
        Html.p "There was an error processing this manual"
