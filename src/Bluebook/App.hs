module Bluebook.App
    ( AppT
    , runAppT
    ) where

import Bluebook.Prelude

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
