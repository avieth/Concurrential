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
read :: String -> ConcurrentialAp (Maybe String)
read = concurrently . readIO

-- | Writes must be executed sequentially, else the programmer's intent will not
--   be respected: if two or more writes to the same key are staged, then the
--   one which appears later in the term must overwrite the earlier one.
write :: String -> Maybe String -> ConcurrentialAp ()
write key = sequentially . writeIO key

-- | Run this IO to see that the reads do indeed happen concurrently (the
--   printed text is interleaved and unreadable) but notice that the writes
--   are always performed in-order. In particular, the second write to "A" will
--   always prevail.
demonstration :: IO [Maybe String]
demonstration = runConcurrential . concurrentially $
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
data Concurrential t where
  SCAtom :: Either (Sequential t) (Concurrent t) -> Concurrential t
  SCBind :: Concurrential s -> (s -> Concurrential t) -> Concurrential t
  SCAp :: Concurrential (r -> t) -> Concurrential r -> Concurrential t
```

Given the `SCBind` constructor, definition of `>>=` is trivial:

```Haskell
(>>=) :: Concurrential s -> (s -> Concurrential t) -> Concurrential t
(>>=) = SCBind
```

The applicative `<*>` is more involved. Whereas the `Concurrently` monad from
`Control.Concurrent.Async` evaluates both arguments of `<*>` concurrently,
the applicative for `Concurrential` does not! This is to guarantee that

  `mf <*> mx = mf `ap` mx = mf >>= (\f -> mx >>= \x -> return (f x))`

which some might expect of any monad. To gain concurrency in `Concurrential`,
we define `ConcurrentialAp`, which is a newtype over `Concurrential`, but
which is *not* a monad! This type's applicative instance uses the `SCAp`
constructor. When interpreted by `runConcurrential`, it runs its arguments
concurrently.

`return` and `pure` use the sequential variant of `SCAtom`.

With this language in place, the function `runConcurrential` is defined, which
will run the `m`s in the `Concurrential` term concurrently such that all
sequential `SCAtom` terms are run in the order in which they appear.
