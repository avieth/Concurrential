Concurrential
=============

The `Concurrently` monad from async, when used as an applicative functor, will
compute both of its terms concurrently. The `Concurrential` monad, when used as
an applicative functor, *may* compute both of its terms concurrently, depending
upon their structure. Whereas `Concurrently` allows the programmer to indicate
that two computations should be run concurrently, `Concurrential` allows the
programmer to indicate that two computations should be run concurrently *as
much as possible*, and precisely where the concurrency is possible or not
possible. This increases modularity, as the order of computations can be fixed
to a certain extent, without demanding that the user of this computation know
anything about its concurrency-related assumptions.

# A demonstration

In this example, we assume some IO read/write feature, perhaps to and from a
database system, and show how reads and writes can be safely interleaved.

```Haskell
import Prelude hiding (read, readIO)
import Control.Applicative
import Control.Concurrent.Concurrential

-- | Read the value at some key. We use strings for simplicity, and forego a
--   reasonable implementation.
readIO :: String -> IO (Maybe String)
readIO bs = putStrLn ("Reading: " ++ bs) >> return Nothing

-- | Write the value at some key. We use strings for simplicity, and forego a
--   reasonable implementation.
writeIO :: String -> Maybe String -> IO ()
writeIO key value = putStrLn ("Writing: " ++ key ++ ", " ++ show value)

-- | Reads may be executed concurrently.
read :: String -> Concurrential IO (Maybe String)
read = concurrently . readIO

-- | Writes must be executed sequentially, else the programmer's intent will not
--   be respected: if two or more writes to the same key are staged, then the
--   one which appears later in the term must overwrite the earlier one.
write :: String -> Maybe String -> Concurrential IO ()
write key = sequentially . writeIO key

-- | Run this IO to see that the reads do indeed happen concurrently (the
--   printed text is interleaved and unreadable) but notice that the writes
--   are always performed in-order. In particular, the second write to "A" will
--   always prevail.
demonstration :: IO [Maybe String]
demonstration = runConcurrentialSimple $
        (\x y z -> [x, y, z])
    <$> read "A"
    <*  write "A" (Just "z")
    <*  write "B" (Just "b")
    <*> read "B"
    <*  write "C" (Just "c")
    <*  write "A" (Just "a")
    <*> read "C"
    <*  write "D" (Just "d")
```

# How it works

Terms of the `Concurrently` monad are (approximately) of the form

```Haskell
data Concurrential m t where
  SCAtom :: Either (Sequential (m t)) (Concurrently (m t)) -> Concurrential m t
  SCBind :: Concurrential m s -> (s -> Concurrential m t) -> Concurrential m t
  SCAp :: Concurrential m (r -> t) -> Concurrential m r -> Concurrential m t
```

Given the `SCBind` and `SCAp` constructors, definitions of `>>=` and `<*>` are
trivial:

```Haskell
(<*>) :: Concurrential m (r -> t) -> Concurrential m r -> Concurrential m t
(<*>) = SCAp

(>>=) :: Concurrential m s -> (s -> Concurrential m t) -> Concurrential m t
(>>=) = SCBind
```

`return` and `pure` use `pure :: a -> m a` and inject it into the sequential
variant of `SCAtom`, but I am not sure that choosing the concurrently variant
would have any benefits or drawbacks. The injections `sequentially` and
`concurrently` wrap an existing `m` in the appropriate `SCAtom` variant.

With this language in place, the function `runConcurrential` is defined, which
will run the `m`s in the `Concurrential` term concurrently such that all
sequential `SCAtom` terms are run in the order in which they appear. Of course,
the caller must define how the monad `m` can be injected into `IO` (see the
`Injector` type), but also how an IO inside an `m` can be pulled out front (see
the `Retractor` type). The terms `Injector` and `Retractor` are not standard
and probably not ideal.
