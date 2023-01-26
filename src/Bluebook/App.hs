module Bluebook.App
    ( AppT
    , runAppT
    ) where

import Bluebook.Prelude

import Control.Monad.Catch (MonadCatch, MonadThrow)

newtype AppT env m a = AppT
    { unAppT :: ReaderT env (LoggingT m) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadUnliftIO
        , MonadLogger
        , MonadError e
        , MonadReader env
        )

runAppT :: (MonadUnliftIO m, HasLogger env) => AppT env m a -> env -> m a
runAppT f app = runLoggerLoggingT app $ runReaderT (unAppT f) app
