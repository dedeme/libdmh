-- Copyright 02-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Thread management.
module Dm.Thread
  ( T
  , detached
  , joinable
  , join
  , sleep
  )where

import Control.Concurrent
import Control.Concurrent.MVar

--- Thread reference type.
type T = MVar ()

--- detached fn
--- Execute 'fn' in a detached thread.
detached :: IO () -> IO ()
detached fn = forkIO fn >>= \_ -> return ()

--- joinable fn
--- Execute 'fn' in a joinable thread y returns a reference to use with 'join'.
joinable :: IO () -> IO T
joinable fn = do
  mvar <- newEmptyMVar
  forkFinally fn (\_ -> putMVar mvar ())
  return mvar

--- join ref
--- Stops the current thread until the thread referenced by 'ref' finish.
join :: T -> IO ()
join ref = takeMVar ref >>= \_ -> return ()

--- sleep millis
--- Stops the current thread for 'millis' milliseconds
sleep :: Int -> IO ()
sleep n = threadDelay $ n * 1000
