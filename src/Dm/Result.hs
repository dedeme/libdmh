-- Copyright 24-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Result definition.
--- Result is intended to be imported without qualification.

module Dm.Result
  ( Result
  , rsFromMaybe
  , rsWithFail
  )where

--- Type definition,
type Result = Either String

rsFromMaybe :: String -> Maybe a -> Result a
rsFromMaybe _ (Just v) = Right v
rsFromMaybe msg _ = Left msg

rsWithFail :: Result a -> IO a
rsWithFail (Right a) = return a
rsWithFail (Left e) = fail e

