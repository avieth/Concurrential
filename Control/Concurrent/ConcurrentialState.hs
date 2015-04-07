{-|
Module      : Control.Concurrent.ConcurrentialState
Description : Concurrential with shared state.
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

module Control.Concurrent.ConcurrentialState (

    ConcurrentialState
  , runConcurrentialState

  , sequentialGet
  , concurrentGet
  , sequentialUpdate
  , concurrentUpdate

  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Concurrential
import Data.Typeable

-- | Concurrential with shared state.
newtype ConcurrentialState s a = ConcurrentialState {
    unConcurrentialState :: TVar s -> Concurrential a
  } deriving (Typeable, Functor)

instance Applicative (ConcurrentialState s) where
  pure x = ConcurrentialState $ \tvar -> pure x
  f <*> x = ConcurrentialState $ \tvar ->
      unConcurrentialState f tvar <*> unConcurrentialState x tvar

instance Monad (ConcurrentialState s) where
  return x = ConcurrentialState $ \tvar -> return x
  x >>= k = ConcurrentialState $ \tvar ->
      unConcurrentialState x tvar >>= (flip unConcurrentialState) tvar . k

runConcurrentialState :: s -> ConcurrentialState s a -> IO (a, s)
runConcurrentialState initial term = do
    tvar <- newTVarIO initial
    outcome <- runConcurrential $ unConcurrentialState term tvar
    terminal <- atomically $ readTVar tvar
    return (outcome, terminal)

stateless :: Concurrential a -> ConcurrentialState s a
stateless = ConcurrentialState . const

get :: (forall a . IO a -> Concurrential a) -> ConcurrentialState s s
get makeConcurrential = ConcurrentialState $
    makeConcurrential . atomically . readTVar

sequentialGet :: ConcurrentialState s s
sequentialGet = get sequentially

concurrentGet :: ConcurrentialState s s
concurrentGet = get concurrently

update :: (forall a . IO a -> Concurrential a) -> (s -> s) -> ConcurrentialState s ()
update makeConcurrential step = ConcurrentialState $
    makeConcurrential . atomically . (flip modifyTVar) step

sequentialUpdate :: (s -> s) -> ConcurrentialState s ()
sequentialUpdate = update sequentially

concurrentUpdate :: (s -> s) -> ConcurrentialState s ()
concurrentUpdate = update concurrently

withResult :: (a -> s -> s) -> Concurrential a -> ConcurrentialState s a
withResult updateState term = do
    outcome <- stateless term
    sequentialUpdate (updateState outcome)
    return outcome
