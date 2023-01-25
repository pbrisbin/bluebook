{-# OPTIONS_GHC -Wno-orphans #-}

module Bluebook.Orphans
    () where

import Prelude

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Servant (Handler(..))
import UnliftIO.Exception (Exception, throwIO, try)

instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
    withRunInIO exceptToIO = ExceptT $ try $ do
        withRunInIO $ \runInIO ->
            exceptToIO (runInIO . (either throwIO pure <=< runExceptT))

deriving newtype instance MonadUnliftIO Handler
