-- Copyright 08-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Date utilities.

module Dm.Date(
  Day,
  now,
  new,
  fromStr,
  fromStr',
  fromIso,
  fromIso',
  fromUs,
  fromUs',
  format,
  toStr,
  toIso,
  toUs,
  add,
  df,
  split,
  toJs,
  fromJs
  ) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import qualified Dm.Js as Js
import Dm.Js (JSValue)

-- | @'now'@ - Returns the current date.
now :: IO Day
now = getCurrentTime >>= return . utctDay

-- | @'new' year moth day@ - Creates a new day. First argument is year,
--                           second month number (1-12), third day (1-31).
--
--                           Invalid values will be clipped to the correct
--                           range, month first, then day.
new :: Integer -> Int -> Int -> Day
new = fromGregorian

-- | @'fromStr' s@ Reads a date with format YYYYMMDD. Produces an error if
--                 reading fails.
fromStr :: String -> Day
fromStr s = parseTimeOrError False defaultTimeLocale "%Y%m%d" s

-- | @'fromStr'' s@ Reads a date won format YYYYMMDD. Returns Nothing if fails.
fromStr' :: String -> Maybe Day
fromStr' s = parseTimeM False defaultTimeLocale "%Y%m%d" s

-- | @'fromIso' sp s@ Reads a date with format [D]Dsp[M]MspYYYY. Produces an
--                    error if reading fails.
fromIso :: Char -> String -> Day
fromIso sp s = let tpl = '%':'-':'d':sp:'%':'-':'m':sp:'%':'Y':[]
              in parseTimeOrError False defaultTimeLocale tpl s

-- | @'fromIso'' sp s@ Reads a date with format [D]Dsp[M]MspYYYY. Returns
--                     Nothing if reading fails.
fromIso' :: Char -> String -> Maybe Day
fromIso' sp s = let tpl = '%':'-':'d':sp:'%':'-':'m':sp:'%':'Y':[]
               in parseTimeM False defaultTimeLocale tpl s

-- | @'fromUs' sp s@ Reads a date with format [M]Msp[D]DspYYYY. Produces an
--                   error if reading fails.
fromUs :: Char -> String -> Day
fromUs sp s = let tpl = '%':'-':'m':sp:'%':'-':'d':sp:'%':'Y':[]
              in parseTimeOrError False defaultTimeLocale tpl s

-- | @'fromUs'' sp s@ Reads a date with format [M]Msp[D]DspYYYY. Returns Nothing
--                    if reading fails.
fromUs' :: Char -> String -> Maybe Day
fromUs' sp s = let tpl = '%':'-':'m':sp:'%':'-':'d':sp:'%':'Y':[]
               in parseTimeM False defaultTimeLocale tpl s

-- | @'format' tpl d@ - formats /d/ with template /tpl/. For template see
--   http://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Format.html#v:formatTime
format :: String -> Day -> String
format tpl d = formatTime defaultTimeLocale tpl d

-- | @'toStr' d@ - Returns a string with format YYYYMMDD.
toStr :: Day -> String
toStr = format "%Y%m%d"

-- | @'toIso' sp d@ - Returns a string with format [D]Dsp[M]MspYYYY.
toIso :: Char -> Day -> String
toIso sp d = let tpl = '%':'d':sp:'%':'m':sp:'%':'Y':[]
            in  format tpl d

-- | @'toUs' sp d@ - Returns a string with format [M]Msp[D]DspYYYY.
toUs :: Char -> Day -> String
toUs sp d = let tpl = '%':'m':sp:'%':'d':sp:'%':'Y':[]
            in  format tpl d

-- | @'add' n d@ - Adds /n/ days to /d/.
add :: Int -> Day -> Day
add days n = addDays (fromIntegral days) n

-- | @'df' d1 d2@ - Returns d1 - d2 in days.
df :: Day -> Day -> Int
df d1 d2 = fromIntegral $ diffDays d1 d2

-- | @'split' date@ - Returns (year, month, day) from /date/
split :: Day -> (Integer, Int, Int)
split = toGregorian

-- | @'toJs' date@ - Parses /date/ to JSON.
toJs :: Day -> JSValue
toJs date = let (y, m, d) = split date in Js.wList [
    Js.wInt (fromIntegral y),
    Js.wInt m,
    Js.wInt d
  ]

-- | @'fromJs' js@ - Retrieves a date JSONized.
fromJs :: JSValue -> Day
fromJs js = let [yjs, mjs, djs] = Js.rList js
            in  new (fromIntegral $ Js.rInt yjs) (Js.rInt mjs) (Js.rInt djs)

