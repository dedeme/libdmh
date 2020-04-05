-- Copyright 03-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Time utilities.

module Dm.Time
  ( T
  , now
  , new
  , new'
  , fromUtc
  , fromStr'
  , fromStr
  , toStr
  , stamp
  , add
  , df
  , split
  , split'
  , toJs
  , fromJs
  ) where

import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.Calendar
import qualified Dm.Date as Date
import qualified Dm.Js as Js

--- T
type T = LocalTime

-- now
--- Returns the current time.
now :: IO T
now = do
  ZonedTime lt _ <- getZonedTime
  return lt

--- new year moth day hour min sec
--- Creates a new day. First argument is year, second month number (1-12),
--- third day (1-31), fourth hour, fifth minute and sexth second.
---   - Invalid date values are clipped to the correct range, month first,
---     then day and so on (e.g. 2010 111 230 -> 2010 12 31)
---   - Time values are not clipped.
new :: Int -> Int -> Int -> Int -> Int -> Int -> T
new y mth d h m s =
  LocalTime (Date.new y mth d) $ TimeOfDay h m (fromIntegral s)

new' :: Date.T -> Int -> Int -> Int -> T
new' dt h m s = LocalTime dt $ TimeOfDay h m (fromIntegral s)

--- fromUtc utc
fromUtc :: UTCTime -> LocalTime
fromUtc = utcToLocalTime utc

--- fromStr' d s
--- Reads a time with format HH:MM:SS. Produces an error if reading fails.
fromStr' :: Date.T -> String -> T
fromStr' d s = let LocalTime _ tm = parseTimeOrError False
                                    defaultTimeLocale "%H:%M:%S" s
               in  LocalTime d tm

--- fromStr d s
--- Reads a time with format HH:MM:SS. Returns Nothing if fails.
fromStr :: Date.T -> String -> Maybe T
fromStr d s = let utc = parseTimeM False defaultTimeLocale "%H:%M:%S" s
              in  case utc of
                    Just (LocalTime _ tm) -> Just (LocalTime d tm)
                    _ -> Nothing

--- toStr t
--- Returns a string with format HH:MM:SS.
toStr :: T -> String
toStr = (formatTime defaultTimeLocale "%H:%M:%S")

--- stamp
--- Returns a unique string from the current time.
stamp :: IO String
stamp = show <$> getPOSIXTime

--- add millis t
--- Adds milliseconds to 't'
add :: Int -> T -> T
add millis = (utcToLocalTime utc . addUTCTime (fromIntegral millis / 1000) .
               localTimeToUTC utc)

--- df t1 t2
--- Returns t1 - t2 in milliseconds
df :: T -> T -> Int
df t1 t2 = truncate $
  diffUTCTime (localTimeToUTC utc t1) (localTimeToUTC utc t2) * 1000

--- split time
--- Returns (year, month (1-12), day (1-31), hour, minute, second) from 'time'
split :: T -> (Int, Int, Int, Int, Int, Int)
split (LocalTime day (TimeOfDay h m s)) =
  let (y, mth, d) = toGregorian day
  in  (fromIntegral y, mth, d, h, m, (truncate $ s))

--- split' time
--- Returns (date, hour, minute, second) from 'time'
split' :: T -> (Date.T, Int, Int, Int)
split' (LocalTime day (TimeOfDay h m s)) =
  let (y, mth, d) = toGregorian day
  in  (Date.new (fromIntegral y) mth d, h, m, (truncate $ s))

--- toJs time
--- Parses t to JSON.
toJs :: T -> Js.T
toJs t = let (y, mth, d, h, m, s) = split t in Js.wa [
    Js.wi (fromIntegral y),
    Js.wi mth,
    Js.wi d,
    Js.wi h,
    Js.wi m,
    Js.wi s
  ]

--- fromJs js
--- Retrieves a time JSONized.
fromJs :: Js.T -> Either String T
fromJs js = case Js.ra js of
  Right [yjs, mthjs, djs,
         hjs, mjs, sjs] -> new <$> (fromIntegral <$> Js.ri yjs) <*>
                                   (Js.ri mthjs) <*>
                                   (Js.ri djs) <*>
                                   (Js.ri hjs) <*>
                                   (Js.ri mjs) <*>
                                   (Js.ri sjs)
  _ -> Left "Bad JSON Date"
