-- A tiny example of using Haskell threads and channels
-- Forks several senders and receiver threads who exchange messages
-- main process waits for a few seconds then prints "goodbye" and exits

import Concurrent       -- COS 510 library
import System.Random
import System.IO (hFlush, stdout)
import qualified Data.List as List

type SChan = Chan String

-- Print str to stdout and flush stdout.
putStrNow :: String -> IO ()
putStrNow str = do
  putStr str
  hFlush stdout

-- Wait 1-5 seconds.
delay :: IO ()
delay = do
  n <- randomRIO (1,5)
  threadDelay (n * 1000 * 1000)

-- Wait 1-5 seconds, then send msg on channel c.
sender :: SChan -> String -> IO ()
sender c msg = do
  delay
  writeChan c $ msg ++ "\n"

-- Read one string from c, then print it.
receiver :: SChan -> IO ()
receiver c = do
  x <- readChan c
  putStrNow x

salutations =
  [ "hi"
  , "hey"
  , "hello"
  , "howdy"
  , "sup" ]

main :: IO ()
main = do
  c <- newChan
  let senders = map (sender c) salutations
  let receivers = List.replicate (List.length senders) $ receiver c
  parallel $ receivers ++ senders
  delay
  putStrNow "goodbye\n"
