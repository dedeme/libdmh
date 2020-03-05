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

  putStrLn "  Finised"
