module Bluebook.App
    ( AppT
    , runAppT
    , App
    , loadApp
    ) where

import Bluebook.Prelude

import Bluebook.RenderLink
import Bluebook.Settings

newtype AppT env m a = AppT
    { unAppT :: ReaderT env (LoggingT m) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadUnliftIO
        , MonadLogger
        , MonadError e
        , MonadReader env
        )

runAppT :: (MonadUnliftIO m, HasLogger env) => AppT env m a -> env -> m a
runAppT f app = runLoggerLoggingT app $ runReaderT (unAppT f) app

data App = App
    { appSettings :: Settings
    , appLogger :: Logger
    , appRenderLink :: RenderLink
    }

settingsL :: Lens' App Settings
settingsL = lens appSettings $ \x y -> x { appSettings = y }

instance HasManPath App where
    manPathL = settingsL . manPathL

instance HasLogger App where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasRenderLink App where
    renderLinkL = lens appRenderLink $ \x y -> x { appRenderLink = y }

loadApp :: MonadIO m => Settings -> m App
loadApp settings = do
    let rl = makeRenderLinkWeb $ settingsAppRoot settings
    App settings <$> newLogger (settingsLogSettings settings) <*> pure rl
