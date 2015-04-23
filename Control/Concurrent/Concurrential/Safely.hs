{-|
Module      : Control.Concurrent.Concurrential.Safely
Description : Handle all exceptions in Concurrential computation.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Concurrential.Safely (

    safely
  , runSafely

  ) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent.Except
import Control.Concurrent.Concurrential

injector :: Injector (ExceptT SomeException IO) (Either SomeException)
injector term = runExceptT term >>= return

retractor :: Retractor (Either SomeException)
retractor term = case term of 
    Left e -> return $ Left e
    Right v -> v

-- | Make an arbitrary IO suitable for use with @sequentially@ or @concurrently@
--   so as to produce a term that can be run by @runSafely@:
--
--     let a = concurrently . safely $ dangerousComputation1
--         b = concurrently . safely $ dangerousComputation2
--     in  runSafely $ a *> b
--
safely :: IO a -> ExceptT SomeException IO a
safely io = ExceptT ((Right <$> io) `catch` (\(e :: SomeException) -> return $ Left e))

-- | Run a term such that computation is halted as soon as an exception is
--   encountered, but any pending threads are waited on. The first exception
--   to be thown (in term-order, not necessarily temporal order) is given as
--   Left, and a Right is given if no exception is encountered.
runSafely
  :: Concurrential (ExceptT SomeException IO) a
  -> IO (Either SomeException a)
runSafely c = runConcurrential retractor injector c wait
