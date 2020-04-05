-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module TimeSpec (timeTest) where

import qualified Data.Either as Either
import qualified Dm.Time as Time
import qualified Dm.Date as Date
import Dm.Test

timeTest :: IO ()
timeTest = do
  putStrLn "Time test"

  t0 <- Time.now
  let (y0, m0, d0, _, _, _) = Time.split t0
  let t0' = Time.fromStr' (Date.new y0 m0 d0) (Time.toStr t0)
  teq (Time.split t0) (Time.split t0')

  let t2 = Time.add 2548 t0
  teq (Time.df t2 t0) 2548

  let t = Either.fromRight t2 (Time.fromJs $ Time.toJs t0)
  teq (Time.split t) (Time.split t0)

  dt <- Date.now
  let ta = Time.new' dt 3 15 30
  let (y1, m1, d1) = Date.split dt
  let tb = Time.new y1 m1 d1 3 15 30
  teq ta tb
  tneq (Time.new' dt 40 110 112) $ Time.new' dt 24 59 59
  let (y2, mt2, d2, h2, m2, s2) = Time.split ta
  tyes $ (Time.df ta $ Time.new y2 mt2 d2 h2 m2 s2) == 0
  let (dt2, _, _, _) = Time.split' ta
  tyes $ dt == dt2

  putStrLn "  Finised"
