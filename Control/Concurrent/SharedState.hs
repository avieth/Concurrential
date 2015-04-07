{-|
Module      : Control.Concurrent.SharedState
Description : Shared state transformer for some concurrent IO-capable monad.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.SharedState (

    SharedStateT
  , runSharedStateT

  , stateless
  , get
  , update

  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Concurrential
import Data.Typeable

-- | Introduce shared state into a monad.
newtype SharedStateT s m a = SharedStateT {
    unSharedStateT :: TVar s -> m a
  } deriving (Typeable, Functor)

instance Applicative m => Applicative (SharedStateT s m) where
  pure x = SharedStateT $ \tvar -> pure x
  f <*> x = SharedStateT $ \tvar ->
      unSharedStateT f tvar <*> unSharedStateT x tvar

instance Monad m => Monad (SharedStateT s m) where
  return x = SharedStateT $ \tvar -> return x
  x >>= k = SharedStateT $ \tvar ->
      unSharedStateT x tvar >>= (flip unSharedStateT) tvar . k

runSharedStateT
  :: Monad m
  => (forall a . IO a -> m a)
  -> s
  -> SharedStateT s m a
  -> m (a, s)
runSharedStateT liftIO initial term = do
    tvar <- liftIO $ newTVarIO initial
    outcome <- unSharedStateT term tvar
    terminal <- liftIO . atomically $ readTVar tvar
    return (outcome, terminal)

stateless :: m a -> SharedStateT s m a
stateless = SharedStateT . const

get :: (forall a . IO a -> m a) -> SharedStateT s m s
get liftIO = SharedStateT $ liftIO . atomically . readTVar

update :: (forall a . IO a -> m a) -> (s -> s) -> SharedStateT s m ()
update liftIO step = SharedStateT $
    liftIO . atomically . (flip modifyTVar) step
