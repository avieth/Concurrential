import Prelude hiding (read, readIO)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Concurrential
import Data.Functor.Identity

readIO :: String -> IO (Maybe String)
readIO bs = putStrLn ("Reading: " ++ bs) >> return Nothing

writeIO :: String -> Maybe String -> IO ()
writeIO key value = putStrLn ("Writing: " ++ key ++ ", " ++ show value)

read :: String -> ConcurrentialAp (Maybe String)
read = concurrently . readIO

write :: String -> Maybe String -> ConcurrentialAp ()
write key = sequentially . writeIO key

demonstration :: ConcurrentialAp [Maybe String]
demonstration
     =  (\x y z -> [x, y, z])
    <$> (read "A" *> (concurrently (threadDelay 5000000) *> read "A"))
    <*  write "A" (Just "a")
    <*  write "B" (Just "b")
    <*> read "B"
    <*  write "C" (Just "c")
    <*> read "C"
    <*  write "D" (Just "d")
