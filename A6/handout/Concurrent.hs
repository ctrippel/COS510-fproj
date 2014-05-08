{-# LANGUAGE RankNTypes #-}

module Concurrent
  ( parallel
  , yield
  , threadDelay
  , Chan
  , newChan
  , readChan
  , writeChan ) where

import Control.Concurrent
  ( forkIO, forkIOWithUnmask, ThreadId, myThreadId 
  , threadDelay, yield, Chan, newChan, readChan, writeChan)

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Exception

-- A version of the standard library [forkFinally] that doesn't
-- mask asynchronous exceptions.

forkFinallyUnmasked :: IO () -> IO () -> IO ThreadId
forkFinallyUnmasked action and_then =
   mask_ $ forkIOWithUnmask $ \unmask -> catch (unmask action) err >> and_then                 
   where err :: IOError -> IO ()                                       
         err e = do { tid <- myThreadId                                
                    ; putStrLn                                         
                      $ "Exception in thread " ++ show tid ++ ": " ++ show e
                    }                                                  
                                                                      

-- Fork a computation that signals its completion by writing to
-- [mvar]. [mvar] is initially empty.  To "join" on the computation
-- (block until it's completed), use [takeMVar mvar], which blocks
-- until [mvar] is filled by the [putMVar] finalizer below.

fork_joinIO :: IO () -> IO (MVar ())
fork_joinIO a 
  = do mvar <- newEmptyMVar
       forkFinallyUnmasked a (putMVar mvar ())
       return mvar


-- Fork a new thread to execute each of the actions in [as]. 
-- Block until all the actions are completed.

parallel :: [IO ()] -> IO ()       
parallel as 
  = case as of
      [] -> return ()
      a : as' -> 
        do mvar <- fork_joinIO a
           parallel as'
           takeMVar mvar


