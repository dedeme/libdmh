-- Copyright 02-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Task sequencer
--- An actor perform tasks sequencially, allowing its synchronization.
--- There is a default Actor to easy sequenciation.
module Dm.Actor
  ( T
  , new
  , send
  , send_
  , sync
  , sync_
  ) where

import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.MVar

--- Actor type
type T = MVar ()

scheduler :: T
scheduler = unsafePerformIO (newEmptyMVar)

--- new
--- Creates a new Actor
new :: IO T
new = newEmptyMVar

--- send actor task
--- Send a new task to be done by 'actor' sequencially.
--- This function ends after performing 'task' and returns its result.
send :: T -> IO a -> IO a
send actor task = do
  putMVar actor ()
  r <- task
  takeMVar actor
  return r

--- send_ actor task
--- Send a new task to be done by 'actor' sequencially.
--- This function ends after performing 'task'.
send_ :: T -> IO () -> IO ()
send_ actor task = do
  putMVar actor ()
  task
  takeMVar actor

--- sync task
--- Send a new task to be done by the default actor sequencially.
--- This function ends after performing 'task' and returns its result.
sync :: IO a  -> IO a
sync = send scheduler

--- sync_ task
--- Send a new task to be done by the default actor sequencially.
--- This function ends after performing 'task'.
sync_ :: IO ()  -> IO ()
sync_ = send_ scheduler
