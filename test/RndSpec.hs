-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module RndSpec where

import Dm.Test
import Dm.Rnd as Rnd

rndTest :: IO ()
rndTest = do
  putStrLn "Testing Rnd"

  (fmap (1 >) Rnd.d) >>= tyes
  (fmap (0 <=) Rnd.d) >>= tyes

  (fmap (0 ==) (Rnd.i (-1))) >>= tyes
  (fmap (2 >) (Rnd.i 2)) >>= tyes
  (fmap (0 <=) (Rnd.i 2)) >>= tyes
  Rnd.i 1 >>= \n -> teq n 0
{-
  Rnd.dList 3 >>= print
  Rnd.iList 3 22 >>= print
  Rnd.shuffle ([]::[Int]) >>= \l -> teq l []
  Rnd.shuffle ([1]::[Int]) >>= \l -> teq l [1]
  Rnd.shuffle [1::Int, 2, 3] >>= print
  showBox 5 $ box [1::Int, 2, 3]
  showBox 5 $ box' [(2, 1::Int), (1, 2)]
-}

  putStrLn "  Finished"
{-
  where
    showBox :: Int -> Box Int-> IO ()
    showBox 0 _ = return ()
    showBox n bx = do
      (v, bx') <- Rnd.boxNext bx
      print [n, v]
      showBox (n - 1) bx'
-}
