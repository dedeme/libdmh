-- Copyright 23-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Test functions.

module Dm.Test
  ( tyes
  , tnot
  , teq
  , tneq
  , tput
  ) where

import Debug.Trace

msg :: String -> String -> String
msg a e = "  Test failed.\n  Expected: " ++ e ++ "\n  Actual  : " ++ a

--- tyes v
--- Fails if v is 'False'
tyes :: Bool -> IO ()
tyes v = if v then return ()
              else fail $ msg "False" "True"

--- tnot v
--- Fails if v is 'True'
tnot :: Bool -> IO ()
tnot v = if v then fail $ msg "True" "False"
              else return ()

--- teq e1 e2
--- Fails if 'a /= e' (actual /= expected)
teq :: (Show a, Eq a) => a -> a -> IO ()
teq a e = if a == e then return ()
                    else fail $ msg (show a) (show e)

--- tneq e1 e2
--- Fails if 'e1 == e2' (actual == expected)
tneq :: (Show a, Eq a) => a -> a -> IO ()
tneq a e = if a == e then fail $ msg ("/= " ++ (show a)) (show a)
                     else return ()

tput :: Show a => a -> a
tput = traceShowId
