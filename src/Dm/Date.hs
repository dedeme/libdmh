-- Copyright 08-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Date utilities.

module Dm.Date
  ( T
  , now
  , new
  , fromStr
  , fromStr'
  , fromIso
  , fromIso'
  , fromUs
  , fromUs'
  , format
  , toStr
  , toIso
  , toUs
  , add
  , df
  , split
  , toJs
  , fromJs
  ) where

import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import qualified Dm.Js as Js
import Dm.Result

--- T
type T = Day

--- now
--- Returns the current day.
now :: IO T
now = do
  ZonedTime (LocalTime d _) _ <- getZonedTime
  return d

--- new year moth day
--- Creates a new day. First argument is year, second month number (1-12),
--- third day (1-31).
---   - Invalid values will be clipped to the correct range, month first,
---     then day (e.g. new 2010 111 230 -> new 2010 12 31)
new :: Int -> Int -> Int -> T
new y = (fromGregorian (fromIntegral y))

--- fromStr' s
--- Reads a day with format YYYYMMDD. Produces an error if reading fails.
fromStr' :: String -> T
fromStr' s = parseTimeOrError False defaultTimeLocale "%Y%m%d" s

--- fromStr s
--- Reads a day with format YYYYMMDD. Returns Nothing if fails.
fromStr :: String -> Maybe T
fromStr s = parseTimeM False defaultTimeLocale "%Y%m%d" s

--- fromIso' sp s
--- Reads a day with format [D]Dsp[M]MspYYYY. Produces an error if reading
--- fails.
fromIso' :: Char -> String -> T
fromIso' sp s = let tpl = '%':'-':'d':sp:'%':'-':'m':sp:'%':'Y':[]
              in parseTimeOrError False defaultTimeLocale tpl s

--- fromIso sp s
--- Reads a day with format [D]Dsp[M]MspYYYY. Returns Nothing if reading fails.
fromIso :: Char -> String -> Maybe T
fromIso sp s = let tpl = '%':'-':'d':sp:'%':'-':'m':sp:'%':'Y':[]
               in parseTimeM False defaultTimeLocale tpl s

--- fromUs' sp s
--- Reads a day with format [M]Msp[D]DspYYYY. Produces an error if reading
--- fails.
fromUs' :: Char -> String -> T
fromUs' sp s = let tpl = '%':'-':'m':sp:'%':'-':'d':sp:'%':'Y':[]
              in parseTimeOrError False defaultTimeLocale tpl s

--- fromUs sp s
--- Reads a day with format [M]Msp[D]DspYYYY. Returns Nothing if reading fails.
fromUs :: Char -> String -> Maybe T
fromUs sp s = let tpl = '%':'-':'m':sp:'%':'-':'d':sp:'%':'Y':[]
               in parseTimeM False defaultTimeLocale tpl s

--- format tpl d
--- Formats 'd' with template 'tpl'. For template see:
---   - http://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Format.html#v:formatTime
format :: String -> T -> String
format tpl d = formatTime defaultTimeLocale tpl d

--- toStr d
--- Returns a string with format YYYYMMDD.
toStr :: T -> String
toStr = format "%Y%m%d"

--- toIso sp d
--- Returns a string with format [D]Dsp[M]MspYYYY.
toIso :: Char -> T -> String
toIso sp d = let tpl = '%':'d':sp:'%':'m':sp:'%':'Y':[]
            in  format tpl d

--- toUs sp d
--- Returns a string with format [M]Msp[D]DspYYYY.
toUs :: Char -> T -> String
toUs sp d = let tpl = '%':'m':sp:'%':'d':sp:'%':'Y':[]
            in  format tpl d

--- add n d
--- Adds 'n' days to 'd'.
add :: Int -> T -> T
add days d = addDays (fromIntegral days) d

--- df d1 d2
--- Returns d1 - d2 in days.
df :: T -> T -> Int
df d1 d2 = fromIntegral $ diffDays d1 d2

--- split day
--- Returns (year, month (1-12), day (1-31)) from 'day'
split :: T -> (Int, Int, Int)
split day = let (y, m, d) = toGregorian day in (fromIntegral y, m, d)

--- toJs day
--- Parses day to JSON.
toJs :: T -> Js.T
toJs day = let (y, m, d) = split day in Js.wa [
    Js.wi (fromIntegral y),
    Js.wi m,
    Js.wi d
  ]

--- fromJs js
--- Retrieves a day JSONized.
fromJs :: Js.T -> Result T
fromJs js = case Js.ra js of
  Right [yjs, mjs, djs] -> new <$> (fromIntegral <$> Js.ri yjs) <*>
                                   (Js.ri mjs) <*>
                                   (Js.ri djs)
  _ -> Left "Bad JSON Date"

