{-|
Module      : Control.Concurrent.SafeConcurrential
Description : Concurrential with automatic exception management.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

The definitions from Control.Concurrent.Concurrential allow the
programmer to tag arbitrary IOs as sequential or possibly concurrent, and to
achieve automatic conurrency via Applicative combinators, but they leave the
programmer responsible for exception handling.
This module builds atop Concurrential by facilitating exception-safe
concurrency, in which an exception in any thread will stop further computation,
but will wait for any pending computations to finish, gathering exceptions
observed in all threads for inspection outside of the SafeConcurrential monad.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.SafeConcurrential (

    SafeConcurrential
  , runSafeConcurrential
  , safeSequentially
  , safeConcurrently

  ) where

import Control.Applicative
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Concurrent.Concurrential as C
import Data.Typeable

newtype ExceptionConcurrential a = ExceptionConcurrential {
    unExceptionConcurrential :: C.Concurrential (Either SomeException a)
  } deriving (Typeable, Functor)

instance Applicative ExceptionConcurrential where
  pure = ExceptionConcurrential . pure . pure
  f <*> x = ExceptionConcurrential $
      takeEither <$> (unExceptionConcurrential f) <*> (unExceptionConcurrential x)
    where
      takeEither eitherF eitherX = eitherF <*> eitherX

instance Monad ExceptionConcurrential where
  return = ExceptionConcurrential . return . return
  x >>= k = ExceptionConcurrential $ do
      eitherX <- unExceptionConcurrential x
      case eitherX of
        Left e -> return $ Left e
        Right x -> unExceptionConcurrential $ k x

newtype SafeConcurrential s a = SafeConcurrential {
    unSafeConcurrential :: TVar s -> ExceptionConcurrential a
  } deriving (Typeable, Functor)

instance Applicative (SafeConcurrential s) where
  pure x = SafeConcurrential $ \tvar -> pure x
  f <*> x = SafeConcurrential $ \tvar ->
      unSafeConcurrential f tvar <*> unSafeConcurrential x tvar

instance Monad (SafeConcurrential s) where
  return x = SafeConcurrential $ \tvar -> return x
  x >>= k = SafeConcurrential $ \tvar ->
      unSafeConcurrential x tvar >>= (flip unSafeConcurrential) tvar . k

runSafeConcurrential :: s -> SafeConcurrential s a -> IO (Either SomeException a, s)
runSafeConcurrential initial sc = do
    tvar <- newTVarIO initial
    outcome <- C.runConcurrential . unExceptionConcurrential $ unSafeConcurrential sc tvar
    terminal <- atomically $ readTVar tvar
    return (outcome, terminal)

makeSafeConcurrential
  :: (forall a . IO a -> C.Concurrential a)
  -> (SomeException -> s -> s)
  -> IO a
  -> SafeConcurrential s a
makeSafeConcurrential makeConcurrential onException io = SafeConcurrential $ \tvar ->
    let io' = (Right <$> io) `catch` handler
        handler = \(e :: SomeException) -> do
                     atomically . modifyTVar tvar $ onException e
                     return $ Left e
    in  ExceptionConcurrential . makeConcurrential $ io'

safeSequentially :: (SomeException -> s -> s) -> IO a -> SafeConcurrential s a
safeSequentially = makeSafeConcurrential C.sequentially

safeConcurrently :: (SomeException -> s -> s) -> IO a -> SafeConcurrential s a
safeConcurrently = makeSafeConcurrential C.concurrently
