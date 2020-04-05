-- Copyright 02-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Task sequencer
--- An actor perform tasks sequencially, allowing its synchronization.
--- There is a default Actor to easy sequenciation.
module Dm.Actor
  ( T
  , new
  , send
  , send'
  ) where

import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.MVar

--- Actor type
type T = MVar ()

--- new
--- Creates a new Actor
new :: IO T
new = newEmptyMVar

--- send actor task
--- Sends a new task to be done by 'actor' sequencially.
--- This function ends after performing 'task' and returns its result.
send :: T -> IO a -> IO a
send actor task = do
  putMVar actor ()
  r <- task
  takeMVar actor
  return r

--- send' actor task
--- Sends a new task to be done by 'actor' sequencially.
--- This function ends after performing 'task'.
send' :: T -> IO () -> IO ()
send' actor task = do
  putMVar actor ()
  task
  takeMVar actor
