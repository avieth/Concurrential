{-|
Module      : Control.Concurrent.Except
Description : Just like ExceptT from transformers but with a different Applicative
              instance.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.Except (

    ExceptT(..)
  , throwE

  ) where

import Control.Applicative
import Data.Typeable

data ExceptT e m a = ExceptT {
    runExceptT :: m (Either e a)
  } deriving (Typeable)

instance Functor m => Functor (ExceptT e m) where
  fmap f term = ExceptT $ (fmap . fmap) f (runExceptT term)

instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . pure
  f <*> x = ExceptT $ (<*>) <$> runExceptT f <*> runExceptT x

instance Monad m => Monad (ExceptT e m) where
  return = ExceptT . return . return
  x >>= k = ExceptT $ do
      outcome <- runExceptT x
      case outcome of
        Left e -> return $ Left e
        Right x -> runExceptT $ k x

throwE :: Applicative m => e -> ExceptT e m a
throwE = ExceptT . pure . Left
