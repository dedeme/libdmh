-- Copyright 11-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Ramdom utilities.

module Dm.Rnd(
  Box,
  i,
  d,
  iList,
  dList,
  shuffle,
  box,
  boxNext,
  boxDo
  ) where

import System.Random
import Data.List
import Control.Applicative

-- | @'i' max@ - Produces a random integer between [0-max)
i :: Int -> IO(Int)
i max = randomRIO (0, max - 1)

-- | @'d'@ - Produces a random Double between [0-1)
d :: IO(Double)
d = randomIO

-- | @'iList' max@ - Procues a list with random integers between [0-max)
iList :: Int -> IO([Int])
iList max = newStdGen >>= return . (randomRs (0, max - 1))

-- | @'dList'@ - Produces a list with random doubles between [0-1)
dList :: IO([Double])
dList = newStdGen >>= return . randoms

-- | @'shuffle' ls@ - Returns a list with elements of /ls/ randomly ordered.
shuffle :: [a] -> IO [a]
shuffle ls = sh (length ls) ls
  where
  sh _ [] = return []
  sh _ [e] = return [e]
  sh len ls = do
    ix <- i len
    let (left, (x:xs)) = splitAt ix ls
    rest <- sh (len - 1) (left ++ xs)
    return (x:rest)

-- | @'Box' [base] [current]@ - Is a Type to generate infinite random
--                          ocurrences of elements from a list.
--
--                          At the beginning /base/ is reordered randomly in
--                          /current/
--
--                          When 'boxNext' is called the head of /current/ is
--                          returned an removed. When /curreent/ is exhausted,
--                          is generated from /base/ again.
data Box a = Box [a] [a]

-- | @'box' ls@ - Creates a box with elements of /ls/
box :: [a] -> Box a
box ls = Box ls []

-- | @'box' [(n, e)]@ - Creates a box with elements /e/'s duplicated /n/ times.
box' :: [(Int, a)] -> Box a
box' ls = Box (foldl' (\r (n, e) -> (replicate n e) ++ r) [] ls) []

-- | @'boxNext' box@ - Returns the next element of /box/
boxNext :: Box a -> IO(a, Box a)
boxNext (Box d curr) = do
  (x:xs) <- if null curr then shuffle d else return curr
  return (x, Box d xs)

-- | @'boxDo' bx f@ - Runs /f/ with elements of /bx/ until it returns
--                    @IO False@.
--
-- >>> bx = box [1, 2, 3]
-- >>> f n = do print n; tx <- getLine; return (tx == "more")
-- >>> boxDo bx f
-- 1
-- more
-- 3
-- more
-- 2
-- more
-- 3
-- end
-- >>>
boxDo :: Box a -> (a -> IO Bool) -> IO ()
boxDo bx f = do
  (e, bx') <- boxNext bx
  cont <- f e
  if cont then boxDo bx' f else return ()


