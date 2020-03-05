-- Copyright 11-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Ramdom utilities.

module Dm.Rnd(
  Box,
  i,
  d,
  iList,
  dList,
  shuffle,
  box,
  box',
  boxNext
  ) where

import System.Random
import Data.List
import Control.Applicative

--- d
--- Returns a value between (0 - 1]
d :: IO(Double)
d = randomIO

--- i
--- Returns a value between (0 - mx]
i :: Int -> IO(Int)
i max = if max < 2 then return 0 else randomRIO (0, max - 1)

--- dList n
--- Creates a list with 'n' random numbers between (0 - 1].
dList :: Int -> IO([Double])
dList n = foldl
          (\r -> \e -> do rv <- r; ev <- e; return (ev:rv))
          (return []) (reverse $ take n (repeat d))

--- iList n
--- Creates a list with 'n' random numbers (0 - mx].
iList :: Int -> Int -> IO([Int])
iList n mx = foldl
        (\r -> \e -> do rv <- r; ev <- e; return (ev:rv))
        (return []) (reverse $ take n $ repeat $i mx)

--- shuffle ls
--- Returns a list with elements of 'ls' randomly sorted.
shuffle :: [a] -> IO [a]
shuffle l = sh (return []) l
  where
    sh r [] = r
    sh r ls = do
      rl <- r
      ix <- i $ length ls
      sh (return ((ls!!ix):rl)) ((take ix ls) ++ (drop (ix + 1) ls))

--- Object to return random values.
--- Values belong to a list and returned with the following procedure:
---   1.  Returns every element in random order.
---   2. Restore the original list and go to step 1.
--- Box does not export its constructor (use instead "box" and "box'").
data Box a = Box [a] [a]

--- box ls
--- Creates a box with elements of 'ls'
box :: [a] -> Box a
box ls = Box ls []

--- box' ls
--- Creates a box with elements of 'ls'. Each element has a number 'n' and
--- a value, indicateing that it will be added 'n' elements of such value.
box' :: [(Int, a)] -> Box a
box' ls = Box (foldl (\r (n, e) -> (replicate n e) ++ r) [] ls) []

--- boxNext box
--- Returns the next element of 'box'.
boxNext :: Box a -> IO (a, Box a)
boxNext (Box base curr) = do
  (x:xs) <- if null curr then shuffle base else return curr
  return (x, Box base xs)
