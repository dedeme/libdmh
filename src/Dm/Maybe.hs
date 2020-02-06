-- Copyright 24-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Maybe functions.

module Dm.Maybe where

import qualified Data.Maybe as M

--- isJust v
--- Returns 'true' if 'v' is a 'Just' value.
isJust :: Maybe a -> Bool
isJust = M.isJust

--- isNothing v
--- Returns 'true' if 'v' is a 'Nothing' value.
isNothing :: Maybe a -> Bool
isNothing = M.isNothing

--- fromJust v
--- Returns 'x' if 'v' is 'Just x' or fails if 'v' is 'Nothing'
fromJust :: Maybe a -> a
fromJust = M.fromJust

--- fromMaybe d v
--- Returns 'x' if 'v' is 'Just x' or d if 'v' is 'Nothing'
fromMaybe :: a -> Maybe a -> a
fromMaybe = M.fromMaybe

--- withFail v msg
--- Transform a 'Maybe' value to functional value with the following rules:
---   - If 'v' is 'Just x', returns 'x'
---   - Otherwise raise a fail with the message of 'msg'
withFail :: Maybe a -> String -> a
withFail (Just v) _ = v
withFail _ msg = error msg
