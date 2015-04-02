{-|
Module      : Control.Concurrent.Concurrential
Description : Description of concurrent computation with sequential components. 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}

module Control.Concurrent.Concurrential (

    Concurrential

  , runConcurrential

  , sequentially
  , concurrently

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async hiding (concurrently)

-- | Description of the way in which an IO should be carried out.
data Choice t = Sequential (IO t) | Concurrent (IO t)

instance Functor Choice where
  fmap f choice = case choice of
      Sequential io -> Sequential $ fmap f io
      Concurrent io -> Concurrent $ fmap f io

-- | Description of computation which is composed of sequential and concurrent
--   parts.
data Concurrential t where
  SCAtom :: Choice t -> Concurrential t
  SCBind :: Concurrential s -> (s -> Concurrential t) -> Concurrential t
  SCAp :: Concurrential (r -> t) -> Concurrential r -> Concurrential t

instance Functor Concurrential where
  fmap f sc = case sc of
    SCAtom choice -> SCAtom $ fmap f choice
    SCBind sc k -> SCBind sc ((fmap . fmap) f k)
    SCAp sf sx -> SCAp ((fmap . fmap) f sf) sx

instance Applicative Concurrential where
  pure = SCAtom . Sequential . pure
  (<*>) = SCAp

instance Monad Concurrential where
  return = pure
  (>>=) = SCBind

-- | Run a Concurrential term with a continuation. We choose CPS here because
--   it allows us to explot @withAsync@, giving us a guarantee that an
--   exception in a spawning thread will kill spawned threads.
runConcurrentialK :: Concurrential t -> (Async t -> IO r) -> IO r
runConcurrentialK sc k = case sc of
    SCAtom choice -> case choice of
        Sequential io -> io >>= \x -> withAsync (return x) k
        Concurrent io -> withAsync io k
    SCBind sc next -> runConcurrentialK sc $ \asyncS -> do
        s <- wait asyncS
        runConcurrentialK (next s) k
    SCAp left right ->
        runConcurrentialK left $ \asyncF ->
        runConcurrentialK right $ \asyncX ->
        let waitAndApply = do
              f <- wait asyncF
              x <- wait asyncX
              return $ f x
        in withAsync waitAndApply k

-- | Run a Concurrential term, realizing the effects of the IOs which compose
--   it.
runConcurrential :: Concurrential t -> IO t
runConcurrential c = runConcurrentialK c wait

-- | Create an IO which must be run sequentially.
--   If a @sequentially io@ appears in a @Concurrential t@ term then it will
--   always be run to completion before any later part of the term is run.
--   Consider the following terms:
--
--   @
--     a = someConcurrential *> sequentially io *> someOtherConcurrential
--     b = someConcurrential *> concurrently io *> someOtherConcurrential
--   @
--
--   When running the term @a@, we are guaranteed that @io@ is completed before
--   @someOtherConcurrential@ is begun, but when running the term @b@, this is
--   not the case; @io@ may be interleaved with or even run after
--   @someOtherConcurrential@ is run.
sequentially :: IO t -> Concurrential t
sequentially = SCAtom . Sequential

-- | Create an IO which is run concurrently where possible, i.e. whenever it
--   combined applicatively with other terms. For instance:
--
--   @
--     a = concurrently io *> someConcurrential
--     b = concurrently io >> someConcurrential
--   @
--
--   When running the term @a@, the IO term @io@ will be run concurrently with
--   @someConcurrential@, but not so in @b@, because monadic composition has
--   been used.
concurrently :: IO t -> Concurrential t
concurrently = SCAtom . Concurrent
