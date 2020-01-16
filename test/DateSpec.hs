module DateSpec (dateTest) where

import Data.Time.Calendar
import qualified Dm.Date as Date
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

dateTest :: IO ()
dateTest = do
  d0 <- Date.now
  putStrLn $ show d0
  putStrLn $ showGregorian d0
  let d = Date.new 2010 04 02
  let d1 = Date.new 2013 3 1
  let d2 = Date.new 2013 3 6
  let d3 = Date.new 2013 4 30
  putStrLn "Date test"
  putStrLn $ alist [
      (assert, (length $ Date.toStr $ d0) == 8),
      (assert, Date.toStr d == "20100402"),
      (assert, Date.toIso '/' d == "02/04/2010"),
      (assert, Date.toUs '/' d == "04/02/2010"),
      (assert, (Date.toStr . Date.fromStr) "19881231" == "19881231"),
      (assert,
        ((Date.toUs '/') . (Date.fromUs '/')) "12/31/1988" == "12/31/1988"),
      (assert,
        ((Date.toIso '/') . (Date.fromIso '/')) "31/12/1988" == "31/12/1988"),
      (assert, (Date.fromStr' . Date.toStr) d == Just d),
      (assert, ((Date.fromIso' '-') . (Date.toIso '-')) d == Just d),
      (assert, ((Date.fromUs' '-') . (Date.toUs '-')) d == Just d),
      (assert, Date.df d1 d2 == (-5)),
      (assert, Date.df d3 d2 == (55)),
      (assert, d1 < d2),
      (assert, d3 > d2),
      (assert, Date.add (-5) d2 == d1),
      (assert, Date.add 55 d2 == d3),

      (assert, Date.fromIso' '/' "" == Nothing),
      (assert, Date.fromIso' '/' "123/34/34" == Nothing),
      (assert, Date.fromIso' '/' "23/34" == Nothing),
      (assert, Date.fromIso' '/' "23/34/23/34" == Nothing),
      (assert, Date.fromIso' '/' "23/34/234b" == Nothing),
      (assert, Date.fromIso' '/' "23c/34/234b" == Nothing),
      (assert, Date.fromIso' '/' "23/34x/234" == Nothing),

      (assert, Date.fromUs' '/' "" == Nothing),
      (assert, Date.fromUs' '/' "123/34/34" == Nothing),
      (assert, Date.fromUs' '/' "23/34" == Nothing),
      (assert, Date.fromUs' '/' "23/34/23/34" == Nothing),
      (assert, Date.fromUs' '/' "23/34/234b" == Nothing),
      (assert, Date.fromUs' '/' "23c/34/234b" == Nothing),
      (assert, Date.fromUs' '/' "23/34x/234" == Nothing),

      (assert, Date.fromIso '/' "01/02/2015" == Date.fromIso '/' "1/2/2015"),
      (assert, Date.fromUs '/' "02/01/2015" == Date.fromUs '/' "2/1/2015")
    ] ++
    "    Finished"
