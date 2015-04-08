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

  , runConcurrential

  , sequentially
  , concurrently

  , runSimple
  , runExceptionSafe

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async hiding (concurrently)
import Control.Exception
import Data.Functor.Identity
import Data.Typeable

-- | Description of the way in which an IO should be carried out.
data Choice t = Sequential (IO t) | Concurrent (IO t)
  deriving (Typeable)

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
  deriving (Typeable)

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
runConcurrentialK
  :: (Functor m, Applicative m, Monad m)
  => Concurrential t
  -- ^ The computation to run.
  -> Async (m s)
  -- ^ The sequential part.
  -> (forall a . IO a -> IO (m a))
  -- ^ For extensibility
  -> (forall a . m (IO (m a)) -> IO (m a))
  -- ^ For extensibility
  -> (forall s . (Async (m s), Async (m t)) -> IO (m r))
  -- ^ The continuation; fst is sequential part, snd is value part.
  --   We use the rank 2 type for s because we really don't care what the
  --   value of the sequential part it, we just need to wait for it and then
  --   continue with >>.
  -> IO (m r)
runConcurrentialK sc sequentialPart extender retractor k = case sc of
    SCAtom choice -> case choice of
        -- The async created becomes the sequential part and the value
        -- part. So when another Sequential is encountered, its value part
        -- will have to wait for this computation to complete.
        Sequential io -> withAsync
                         (wait sequentialPart >> extender io)
                         (\async -> k (async, async))
        -- The async created is the value part, but the sequential part
        -- remains the same.
        Concurrent io -> withAsync
                         (extender io)
                         (\async -> k (sequentialPart, async))
    SCBind sc next -> runConcurrentialK sc sequentialPart extender retractor $ \(sequentialPart, asyncS) -> do
        s <- wait asyncS
        retractor (fmap (\x -> runConcurrentialK (next x) sequentialPart extender retractor k) s)
    SCAp left right ->
        runConcurrentialK left sequentialPart extender retractor $ \(sequentialPart, asyncF) ->
        runConcurrentialK right sequentialPart extender retractor $ \(sequentialPart, asyncX) ->
        let waitAndApply = do
              f <- wait asyncF
              x <- wait asyncX
              return $ f <*> x
        in  withAsync waitAndApply (\async -> k (sequentialPart, async))

-- | Run a Concurrential term, realizing the effects of the IOs which compose
--   it.
runConcurrential
  :: (Functor m, Applicative m, Monad m)
  => (forall a . IO a -> IO (m a))
  -> (forall a . m (IO (m a)) -> IO (m a))
  -> Concurrential t
  -> IO (m t)
runConcurrential extender retractor c = do
    -- I believe it is safe to supply the async in this way, without using
    -- withAsync, because the computation is trivial, and we need not worry
    -- about this thread dangling.
    sequentialPart <- async $ return (return ())
    runConcurrentialK c sequentialPart extender retractor (wait . snd)

-- | Create an IO which must be run sequentially.
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

runSimple :: Concurrential t -> IO (Identity t)
runSimple = runConcurrential extender retractor
  where
    extender = fmap Identity
    retractor = runIdentity

runExceptionSafe :: Concurrential t -> IO (Either SomeException t)
runExceptionSafe = runConcurrential extender retractor
  where
    extender io = (Right <$> io) `catch` (\(e :: SomeException) -> return $ Left e)
    retractor x = case x of
        Left e -> return $ Left e
        Right io -> io
