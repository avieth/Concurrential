{-|
Module      : Control.Concurrent.Concurrential
Description : Description of concurrent computation with sequential components. 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

The functions @sequentially@ and @concurrently@ inject @IO@ terms into the
@ConcurrentialAp@ applicative functor, whose applicative instance will exploit
as much concurrency as possible such that all @sequentially@ terms will be run
in the order in which they would have been run had they been typical IOs.

Terms of @ConcurrentialAp@ can be transformed into terms of @Concurrential@,
which is a monad. The order of sequential terms is respected even through
binds; a sequential term will not be evaluted until all binds appearing
syntactically earlier than it have been expanded.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Concurrential (

    Concurrential
  , ConcurrentialAp(ConcurrentialAp)

  , runConcurrential

  , sequentially
  , concurrently
  , concurrentially

  , wait

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Async hiding (concurrently)

-- | An Async without a type parameter, which can be waited for.
data SomeAsync where
  SomeAsync :: Async a -> SomeAsync

-- | Wait for a SomeAsync to complete.
waitSomeAsync :: SomeAsync -> IO ()
waitSomeAsync (SomeAsync async) = wait async >> return ()

-- | Our own Identity functor, so that we don't have to depend upon some
--   other package.
newtype Identity a = Identity {
    runIdentity :: a
  } deriving (Functor)

instance Applicative Identity where
  pure = Identity
  f <*> x = Identity $ (runIdentity f) (runIdentity x)

instance Monad Identity where
  return = Identity
  x >>= k = Identity $ (runIdentity . k) (runIdentity x)

-- | Description of the way in which a monadic term's evaluation should be
--   carried out.
data Choice m t = Sequential (m t) | Concurrent (m t)

instance Functor m => Functor (Choice m) where
  fmap f choice = case choice of
      Sequential io -> Sequential $ fmap f io
      Concurrent io -> Concurrent $ fmap f io

-- | Description of computation which is composed of sequential and concurrent
--   parts in some monad @m@.
data Concurrential t where
    SCAtom :: Choice IO t -> Concurrential t
    SCBind :: Concurrential s -> (s -> Concurrential t) -> Concurrential t
    SCAp :: Concurrential (r -> t) -> Concurrential r -> Concurrential t

instance Functor Concurrential where
  fmap f sc = case sc of
    SCAtom choice -> SCAtom $ fmap f choice
    SCBind sc k -> SCBind sc ((fmap . fmap) f k)
    SCAp sf sx -> SCAp ((fmap . fmap) f sf) sx

instance Applicative Concurrential where
  pure = SCAtom . Sequential . pure
  cf <*> cx = SCBind cf (\f -> SCBind cx (\x -> pure (f x)))

instance Monad Concurrential where
  return = pure
  (>>=) = SCBind

-- | Concurrential without a Monad instance, but an Applicative instance
--   which exploits concurrency.
newtype ConcurrentialAp t = ConcurrentialAp {
    unConcurrentialAp :: Concurrential t
  }

instance Functor ConcurrentialAp where
  fmap f sc = ConcurrentialAp $ fmap f (unConcurrentialAp sc)

instance Applicative ConcurrentialAp where
  pure = ConcurrentialAp . pure
  cf <*> cx = ConcurrentialAp $ SCAp (unConcurrentialAp cf) (unConcurrentialAp cx)

-- | Run a Concurrential term with a continuation. We choose CPS here because
--   it allows us to explot @withAsync@, giving us a guarantee that an
--   exception in a spawning thread will kill spawned threads.
runConcurrentialK
  :: Concurrential t
  -- ^ The computation to run.
  -> SomeAsync
  -- ^ The sequential part.
  -> ((SomeAsync, Async t) -> IO r)
  -- ^ The continuation; fst is sequential part, snd is value part.
  -> IO r
runConcurrentialK cc sequentialPart k = case cc of
    SCAtom choice -> case choice of
        -- The async created becomes the sequential part and the value
        -- part. So when another Sequential is encountered, its value part
        -- will have to wait for this computation to complete.
        Sequential em -> withAsync
                         (waitSomeAsync sequentialPart >> em)
                         (\async -> k (SomeAsync async, async))
        -- The async created is the value part, but the sequential part
        -- remains the same.
        Concurrent em -> withAsync
                         (em)
                         (\async -> k (sequentialPart, async))

    SCBind sc next ->
        runConcurrentialK sc sequentialPart $ \(sequentialPart, asyncS) -> do
        synchronizeSequentialPart <- newEmptyMVar
        let waitAndContinue = do
                s <- wait asyncS
                let synchronizeAndWait = \(sequentialPart, valuePart) -> do
                        putMVar synchronizeSequentialPart sequentialPart
                        wait valuePart
                let continue = \x ->
                        runConcurrentialK
                        (next x)
                        sequentialPart
                        synchronizeAndWait
                continue s
        -- This is a very sensitive part of the definition. We fire off a thread
        -- to wait for @asyncS@ and then continue through @next@, but we also
        -- create a thread which blocks until the aforementioned has determined
        -- what is the sequential part of the computation through @next@, as
        -- we need that in order to call the continuation @k@.
        -- We don't actually have to carry the sequential part through; we just
        -- need to create another SomeAsync which waits for that sequential
        -- part. To achieve this, we use an MVar.
        withAsync waitAndContinue $ \async ->
          withAsync (takeMVar synchronizeSequentialPart >>= waitSomeAsync) $ \sequentialPart ->
            k (SomeAsync sequentialPart, async)

    SCAp left right ->
        runConcurrentialK left sequentialPart $ \(sequentialPart, asyncF) ->
        runConcurrentialK right sequentialPart $ \(sequentialPart, asyncX) ->
        let waitAndApply = do
                f <- wait asyncF
                x <- wait asyncX
                return $ f x
        in  withAsync waitAndApply (\async -> k (sequentialPart, async))

-- | Run a Concurrential term, realizing the effects of the IO terms which
--   compose it.
runConcurrential
  :: Concurrential t
  -> (Async t -> IO r)
  -- ^ Similar contract to withAsync; the Async argument is useless outside of
  -- this function.
  -> IO r
runConcurrential cc k = do
    let action = \sequentialPart ->
            runConcurrentialK cc (SomeAsync sequentialPart) (k . snd)
    withAsync (return ()) action

-- | Create an effect which must be run sequentially.
--   If a @sequentially io@ appears in a @Concurrential t@ term then it will
--   always be run to completion before any later sequential part of the term
--   is run. Consider the following terms:
--
--   @
--     a = someConcurrential *> sequentially io *> someOtherConcurrential
--     b = someConcurrential *> concurrently io *> someOtherConcurrential
--     c = someConcurrential *> sequentially io *> concurrently otherIo
--   @
--
--   When running the term @a@, we are guaranteed that @io@ is completed before
--   any sequential part of @someOtherConcurrential@ is begun, but when running
--   the term @b@, this is not the case; @io@ may be interleaved with or even
--   run after any part of @someOtherConcurrential@. The term @c@ highlights an
--   important point: @concurrently otherIo@ may be run before, during or after
--   @sequentially io@! The ordering through applicative combinators is
--   guaranteed only among sequential terms.
sequentially :: IO t -> ConcurrentialAp t
sequentially = ConcurrentialAp . SCAtom . Sequential

-- | Create an effect which is run concurrently where possible, i.e. whenever it
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
concurrently :: IO t -> ConcurrentialAp t
concurrently = ConcurrentialAp . SCAtom . Concurrent

-- | Inject a ConcurrentialAp into Concurrential, losing the
--   concurrency-enabling Applicative instance but gaining a Monad instance.
concurrentially :: ConcurrentialAp t -> Concurrential t
concurrentially = unConcurrentialAp
