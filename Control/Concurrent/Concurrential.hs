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

  , Retractor
  , Injector

  , runConcurrential
  , runConcurrentialSimple

  , sequentially
  , concurrently

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async hiding (concurrently)
import Control.Exception
import Data.Typeable

-- | Description of the way in which a monadic term should be carried out.
data Choice m t = Sequential (m t) | Concurrent (m t)
  deriving (Typeable)

instance Functor m => Functor (Choice m) where
  fmap f choice = case choice of
      Sequential io -> Sequential $ fmap f io
      Concurrent io -> Concurrent $ fmap f io

-- | Description of computation which is composed of sequential and concurrent
--   parts in some monad.
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

-- | This corresponds to the notion of a monad transformer; there is some
--   monad g, and then its associated transformer f. If you have an
--   
--     f m a
--
--   then you can get an
--
--     m (g a)
--
--   just by the definition of what it means to be a monad transformer.
--   Here we're interested in the special case where we can achieve IO (g a).
--   This does not mean we have to be dealing with an f IO a, it could mean
--   that the IO is buried deeper in the transformer stack!
type Injector f g = forall a . f a -> IO (g a)

-- | A witness of this type proves that g is in some sense compatible with IO:
--   we can bind through it.
--   TBD would it suffice to give the simpler type
--     forall a . g (IO a) -> IO (g a)
--   ?
type Retractor g = forall a . g (IO (g a)) -> IO (g a)

-- | Run a Concurrential term with a continuation. We choose CPS here because
--   it allows us to explot @withAsync@, giving us a guarantee that an
--   exception in a spawning thread will kill spawned threads.
runConcurrentialK
  :: (Functor m, Applicative m, Monad m)
  => Retractor m
  -> Injector f m
  -> Concurrential f t
  -- ^ The computation to run.
  -> Async (m s)
  -- ^ The sequential part.
  -> (forall s . (Async (m s), Async (m t)) -> IO (m r))
  -- ^ The continuation; fst is sequential part, snd is value part.
  --   We use the rank 2 type for s because we really don't care what the
  --   value of the sequential part it, we just need to wait for it and then
  --   continue with >>.
  -> IO (m r)
runConcurrentialK retractor injector sc sequentialPart k = case sc of
    SCAtom choice -> case choice of
        -- The async created becomes the sequential part and the value
        -- part. So when another Sequential is encountered, its value part
        -- will have to wait for this computation to complete.
        Sequential em -> withAsync
                         (wait sequentialPart >> injector em)
                         (\async -> k (async, async))
        -- The async created is the value part, but the sequential part
        -- remains the same.
        Concurrent em -> withAsync
                         (injector em)
                         (\async -> k (sequentialPart, async))
    SCBind sc next ->
        runConcurrentialK retractor injector sc sequentialPart $ \(sequentialPart, asyncS) ->
        let waitAndContinue = do
                s <- wait asyncS
                let k' (sequentialPart, asyncT) = wait asyncT
                let continue = \x -> runConcurrentialK retractor injector (next x) sequentialPart k'
                retractor (fmap continue s)
        in  withAsync waitAndContinue (\async -> k (sequentialPart, async))
    SCAp left right ->
        runConcurrentialK retractor injector left sequentialPart $ \(sequentialPart, asyncF) ->
        runConcurrentialK retractor injector right sequentialPart $ \(sequentialPart, asyncX) ->
        let waitAndApply = do
                f <- wait asyncF
                x <- wait asyncX
                return $ f <*> x
        in  withAsync waitAndApply (\async -> k (sequentialPart, async))

-- | Run a Concurrential term, realizing the effects of the IO-like terms which
--   compose it.
runConcurrential
  :: (Functor m, Applicative m, Monad m)
  => Retractor m
  -> Injector f m
  -> Concurrential f t
  -> IO (m t)
runConcurrential retractIO injectIO c = do
    -- I believe it is safe to supply the async in this way, without using
    -- withAsync, because the computation is trivial, and we need not worry
    -- about this thread dangling.
    sequentialPart <- async $ return (return ())
    runConcurrentialK retractIO injectIO c sequentialPart (wait . snd)

runConcurrentialSimple :: Concurrential IO t -> IO t
runConcurrentialSimple = join . runConcurrential retractor injector
  where
    retractor :: Retractor IO
    retractor = join
    injector :: Injector IO IO
    injector io = io >>= return . return
    -- Note that if we chose injector = return we would lose concurrency!
    -- This is very subtle and I don't understand it well.
    -- My best explanation: the injector must bring the effect held in the
    -- term "to the front" so that it would be realized by, for instance, a
    -- withAsync call. If we leave it as just @return@ then runConcurrential
    -- will concurrently build up the term which will ultimately be run
    -- sequentially.

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
sequentially :: m t -> Concurrential m t
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
concurrently :: m t -> Concurrential m t
concurrently = SCAtom . Concurrent

