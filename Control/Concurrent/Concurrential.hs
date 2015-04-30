{-|
Module      : Control.Concurrent.Concurrential
Description : Description of concurrent computation with sequential components. 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

The functions @sequentially@ and @concurrently@ inject @IO@ terms into the
@Concurrential@ monad. This monad's Applicative instance will exploit as
much concurrency as possible, much like the @Concurrently@ monad from async,
such that all @sequentially@ terms will be run in the order in which they
would have been run had they been typical IOs.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Concurrential (

    Concurrential

  , Runner
  , Joiner

  , runConcurrential
  , runConcurrentialSimple

  , sequentially
  , concurrently

  , wait

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Async hiding (concurrently)
import Control.Exception
import Data.Typeable

data SomeAsync where
  SomeAsync :: Async a -> SomeAsync

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
  deriving (Typeable)

instance Functor m => Functor (Choice m) where
  fmap f choice = case choice of
      Sequential io -> Sequential $ fmap f io
      Concurrent io -> Concurrent $ fmap f io

-- | Description of computation which is composed of sequential and concurrent
--   parts in some monad @m@.
data Concurrential m t where
    SCAtom :: Choice m t -> Concurrential m t
    SCBind :: Concurrential m s -> (s -> Concurrential m t) -> Concurrential m t
    SCAp :: Concurrential m (r -> t) -> Concurrential m r -> Concurrential m t
  deriving (Typeable)

instance Functor m => Functor (Concurrential m) where
  fmap f sc = case sc of
    SCAtom choice -> SCAtom $ fmap f choice
    SCBind sc k -> SCBind sc ((fmap . fmap) f k)
    SCAp sf sx -> SCAp ((fmap . fmap) f sf) sx

instance Applicative m => Applicative (Concurrential m) where
  pure = SCAtom . Sequential . pure
  (<*>) = SCAp

instance Applicative m => Monad (Concurrential m) where
  return = pure
  (>>=) = SCBind

-- | This corresponds to the notion of a common type of monad transformer:
--   there is some monad g, and then its associated transformer type f, for
--   instance MaybeT = f and Maybe = g
--   If we have an
--   
--     @
--       f m a
--     @
--
--   then we can get an
--
--     @
--       m (g a)
--     @
--
--   Here we're interested in the special case where we can achieve IO (g a).
--   This does not mean we have to be dealing with an f IO a, it could mean
--   that the IO is buried deeper in the transformer stack!
--
--   Motivation: @Async@ functions work with @IO@ and only @IO@, but the @m@
--   parameter of a Concurrential may be some other monad which is capable of
--   performing @IO@, like @Either String IO@ for instance. In order to run
--   computations in this moand through @Async@, we need to know how to get a
--   hold of an @IO@. That's what the runner does.
type Runner f g = forall a . f a -> IO (g a)

-- | A witness of this type proves that g is in some sense compatible with IO:
--   we can bind through it.
type Joiner g = forall a . g (IO a) -> IO (g a)

-- | Run a Concurrential term with a continuation. We choose CPS here because
--   it allows us to explot @withAsync@, giving us a guarantee that an
--   exception in a spawning thread will kill spawned threads.
runConcurrentialK
  :: (Functor f, Applicative f, Monad f)
  => Joiner f
  -> Runner m f
  -> Concurrential m t
  -- ^ The computation to run.
  -> SomeAsync
  -- ^ The sequential part.
  -> ((SomeAsync, Async (f t)) -> IO (f r))
  -- ^ The continuation; fst is sequential part, snd is value part.
  -> IO (f r)
runConcurrentialK joiner runner sc sequentialPart k = case sc of
    SCAtom choice -> case choice of
        -- The async created becomes the sequential part and the value
        -- part. So when another Sequential is encountered, its value part
        -- will have to wait for this computation to complete.
        Sequential em -> withAsync
                         (waitSomeAsync sequentialPart >> runner em)
                         (\async -> k (SomeAsync async, async))
        -- The async created is the value part, but the sequential part
        -- remains the same.
        Concurrent em -> withAsync
                         (runner em)
                         (\async -> k (sequentialPart, async))

    SCBind sc next ->
        runConcurrentialK joiner runner sc sequentialPart $ \(sequentialPart, asyncS) -> do
        synchronizeSequentialPart <- newEmptyMVar
        let waitAndContinue = do
                s <- wait asyncS
                let synchronizeAndWait = \(sequentialPart, valuePart) -> do
                        putMVar synchronizeSequentialPart sequentialPart
                        wait valuePart
                let continue = \x ->
                        runConcurrentialK
                        joiner
                        runner
                        (next x)
                        sequentialPart
                        synchronizeAndWait
                let unretracted = fmap continue s
                fmap join (joiner unretracted)
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
        runConcurrentialK joiner runner left sequentialPart $ \(sequentialPart, asyncF) ->
        runConcurrentialK joiner runner right sequentialPart $ \(sequentialPart, asyncX) ->
        let waitAndApply = do
                f <- wait asyncF
                x <- wait asyncX
                return $ f <*> x
        in  withAsync waitAndApply (\async -> k (sequentialPart, async))

-- | Run a Concurrential term, realizing the effects of the IO-like terms which
--   compose it.
runConcurrential
  :: (Functor f, Applicative f, Monad f)
  => Joiner f
  -> Runner m f
  -> Concurrential m t
  -> (Async (f t) -> IO (f r))
  -- ^ Similar contract to withAsync; the Async argument is useless outside of
  -- this function.
  -> IO (f r)
runConcurrential joiner runner c k = do
    let action = \sequentialPart ->
            runConcurrentialK joiner runner c (SomeAsync sequentialPart) (k . snd)
    withAsync (return ()) action

runConcurrentialSimple :: Concurrential IO t -> (Async t -> IO r) -> IO r
runConcurrentialSimple c k = runIdentity <$> runConcurrential simpleJoiner simpleRunner c (continue k)

  where

    continue :: (Async t -> IO r) -> (Async (Identity t) -> IO (Identity r))
    continue k = \async -> Identity <$> k (fmap runIdentity async)

    simpleJoiner :: Joiner Identity
    simpleJoiner = fmap Identity . runIdentity

    simpleRunner :: Runner IO Identity
    simpleRunner = fmap Identity

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
--
sequentially :: m t -> Concurrential m t
sequentially = SCAtom . Sequential

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
concurrently :: m t -> Concurrential m t
concurrently = SCAtom . Concurrent
