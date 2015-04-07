{-|
Module      : Control.Concurrent.ExceptionSafe
Description : Automatic exception management for IO-like monads.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Concurrent.ExceptionSafe (

    ExceptionSafeT
  , runExceptionSafeT
  , safely

  ) where

import Control.Applicative
import Control.Exception
import Control.Concurrent.Concurrential
import Control.Concurrent.Except
import Data.Typeable

-- | This is like @ExceptT SomeException Concurrential a@ but we do it
--   manually because the Applicative instance for ExceptT would not be
--   suitable (it would destroy the automatic concurrency).
--   TODO  this one deserves its own module.
newtype ExceptionSafeT m a = ExceptionSafeT {
    unExceptionSafe :: ExceptT SomeException m a
  } deriving (Typeable)

deriving instance Functor m => Functor (ExceptionSafeT m)
deriving instance Applicative m => Applicative (ExceptionSafeT m)
deriving instance Monad m => Monad (ExceptionSafeT m)

runExceptionSafeT :: ExceptionSafeT m a -> m (Either SomeException a)
runExceptionSafeT = runExceptT . unExceptionSafe

safely :: (forall a . IO a -> m a) -> IO a -> ExceptionSafeT m a
safely liftIO io = ExceptionSafeT . ExceptT . liftIO $ safeIO
  where
    safeIO = (Right <$> io) `catch` (\(e :: SomeException) -> return (Left e))
