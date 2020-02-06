-- Copyright 24-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Either and Result functions.

module Dm.Either where

import qualified Data.Either as E

type Result = Either String

--- isLeft v
--- Returns 'True' if 'v' is a 'Left' value.
isLeft :: Either a b -> Bool
isLeft = E.isLeft

--- isRight v
--- Returns 'True' if 'v' is a 'Left' value.
isRight :: Either a b -> Bool
isRight = E.isRight

--- fromLeft d v
--- Returns 'x' if 'v' is 'Left x' or 'd' if 'v' is 'Right'
fromLeft :: a -> Either a b -> a
fromLeft = E.fromLeft

--- fromRight d v
--- Returns 'x' if 'v' is 'Right x' or 'd' if 'v' is 'Left'
fromRight :: b -> Either a b -> b
fromRight =  E.fromRight

--- withFail v
--- Transforms a 'Result' value to a functional value with the following rules:
---   - If 'v' is 'Right x', returns 'x'
---   - Otherwise raises a fail with the message of 'Left'
withFail :: Result a -> a
withFail (Right v) = v
withFail (Left e) = error e

