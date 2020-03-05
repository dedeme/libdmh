-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module DateSpec (dateTest) where

import qualified Dm.Date as Date
import Dm.Test

dateTest :: IO ()
dateTest = do
  d0 <- Date.now

  let d = Date.new 2010 04 02
  let d1 = Date.new 2013 3 1
  let d2 = Date.new 2013 3 6
  let d3 = Date.new 2013 4 30
  putStrLn "Date test"

  teq (length $ Date.toStr $ d0) 8

  --putStrLn "  - from-to (1)"
  teq (Date.toStr d) "20100402"
  teq (Date.toIso '/' d) "02/04/2010"
  teq (Date.toUs '/' d) "04/02/2010"
  teq ((Date.toStr . Date.fromStr') "19881231") "19881231"
  teq (((Date.toUs '/') . (Date.fromUs' '/')) "12/31/1988") "12/31/1988"
  teq (((Date.toIso '/') . (Date.fromIso' '/')) "31/12/1988") "31/12/1988"
  teq ((Date.fromStr . Date.toStr) d) $ Just d
  teq (((Date.fromIso '-') . (Date.toIso '-')) d) $ Just d
  teq (((Date.fromUs '-') . (Date.toUs '-')) d) $ Just d

  --putStrLn "  - Operations"
  teq (Date.df d1 d2) (-5)
  teq (Date.df d3 d2) (55)
  tyes (d1 < d2)
  tyes (d3 > d2)
  teq (Date.add (-5) d2) d1
  teq (Date.add 55 d2) d3

  --putStrLn "  - split"
  teq (Date.split d) (2010, 04, 02)
  teq (Date.split d1) (2013, 3, 1)
  teq (Date.split d2) (2013, 3, 6)
  teq (Date.split d3) (2013, 4, 30)

  --putStrLn "  - from-to (2)"
  teq (Date.fromIso '/' "") Nothing
  teq (Date.fromIso '/' "123/34/34") Nothing
  teq (Date.fromIso '/' "23/34") Nothing
  teq (Date.fromIso '/' "23/34/23/34") Nothing
  teq (Date.fromIso '/' "23/34/234b") Nothing
  teq (Date.fromIso '/' "23c/34/234b") Nothing
  teq (Date.fromIso '/' "23/34x/234") Nothing
  teq (Date.fromUs '/' "") Nothing
  teq (Date.fromUs '/' "123/34/34") Nothing
  teq (Date.fromUs '/' "23/34") Nothing
  teq (Date.fromUs '/' "23/34/23/34") Nothing
  teq (Date.fromUs '/' "23/34/234b") Nothing
  teq (Date.fromUs '/' "23c/34/234b") Nothing
  teq (Date.fromUs '/' "23/34x/234") Nothing
  teq (Date.fromIso' '/' "01/02/2015") $ Date.fromIso' '/' "1/2/2015"
  teq (Date.fromUs' '/' "02/01/2015") $ Date.fromUs' '/' "2/1/2015"

  --putStrLn "  - Js"
  teq ((Date.fromJs . Date.toJs) d) $ Right d

  putStrLn "  Finished"
