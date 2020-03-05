-- Copyright 07-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- JSON module.

module Dm.Js
  ( T
  , fromStr
  , toStr
  , wb
  , wi
  , wd
  , ws
  , wa
  , wo
  , rb
  , ri
  , rd
  , rs
  , ra
  , ro
    -- DERIVATES
  , wMaybe
  , rMaybe
  , wEither
  , rEither
  , wResult
  , rResult
  , wList
  , rList
  , wMap
  , rMap
  ) where

import Text.JSON hiding (Result)
import qualified Dm.Map as Map
import Dm.Result

---
type T = JSValue

emsg :: String -> String
emsg s = "JSON error.\n" ++ s

--- toStr js
--- Returns a string representing 'js'. 'js' must be a List or a Map.
toStr :: T -> String
toStr = encodeStrict

--- fromStr s
--- Returns the JSON object corresponding to 's'.
fromStr :: String -> Result T
fromStr js = case decodeStrict js of
  Ok v -> Right v
  Error e -> Left e

--- wb v
--- Returns the JSON value of 'v'.
wb :: Bool -> T
wb = showJSON

--- wi v
--- Returns the JSON value of 'v'.
wi :: Int -> T
wi = showJSON

--- wd v
--- Returns the JSON value of 'v'.
wd :: Double -> T
wd = showJSON

--- ws v
--- Returns the JSON value of 'v'.
ws :: String -> T
ws = showJSON

--- wa v
--- Returns the JSON value of 'v'.
wa :: [T] -> T
wa = showJSONs

--- wo v
--- Returns the JSON value of 'v'.
wo :: Map.T T -> T
wo ls = JSObject $ toJSObject ls

--- rb js
--- Retuns the value of 'js'.
rb :: T -> Result Bool
rb (JSBool v) = Right v
rb js = Left $ emsg $ "'" ++ (show js) ++ "' is not a Bool"

--- ri js
--- Retuns the value of 'js'.
ri :: T -> Result Int
ri (JSRational _ v) = Right $ truncate v
ri js = Left $ emsg $ "'" ++ (show js) ++ "' is not an Int"

--- rd js
--- Retuns the value of 'js'.
rd :: T -> Result Double
rd (JSRational _ v) = Right $ fromRational v
rd js = Left $ emsg $ "'" ++ (show js) ++ "' is not a Double"

--- rs js
--- Retuns the value of 'js'.
rs :: T -> Result String
rs (JSString v) = Right $ fromJSString v
rs js = Left $ emsg $ "'" ++ (show js) ++ "' is not a String"

--- ra js
--- Retuns the value of 'js'.
ra :: T -> Result [T]
ra (JSArray v) = Right v
ra js = Left $ emsg$ "'" ++ (show js) ++ "' is not an Array"

--- ro js
--- Retuns the value of 'js'.
ro :: T -> Result (Map.T T)
ro (JSObject o) = Right $ fromJSObject o
ro js = Left $ emsg "'" ++ (show js) ++ "' is not an Object"

-- DERIVATES -------------------------------------------------------------------

--- wMaybe f v
--- Writes a Maybe value.
wMaybe :: (a -> T) -> Maybe a -> T
wMaybe f (Just v) = wa [f v]
wMaybe _ Nothing = wa []

--- rMaybe f js
--- Reads a Maybe value.
rMaybe :: (T -> Result a) -> T -> Result (Maybe a)
rMaybe f js = case ra js of
              Right [] -> Right Nothing
              Right [j] -> f j >>= (Right . Just)
              _ -> Left $ emsg $ "'" ++ (show js) ++ "' is not an Maybe"

--- wEither fLeft fRight v
--- Writes a Either value.
wEither :: (a -> T) -> (b -> T) -> Either a b -> T
wEither f _ (Left a) = wa [wb False, f a]
wEither _ f (Right b) = wa [wb True, f b]

--- rEither fLeft fRight js
--- Reads a Either value.
rEither :: (T -> Result a) -> (T -> Result b) -> T ->
           Result (Either a b)
rEither fLeft fRight js =
  case ra js of
    Right [isRight, v] ->
     case rb isRight of
       Right True -> fRight v >>= (Right . Right)
       Right False -> fLeft v >>= (Right . Left)
       _ -> Left msg
    _ -> Left msg
  where
    msg = emsg $ "'" ++ (show js) ++ "' is not an Either"

--- wResult f v
--- Writes a Maybe value.
wResult :: (a -> T) -> Result a -> T
wResult f v = wEither ws f v

--- rResult f js
--- Reads a Maybe value.
rResult :: (T -> Result a) -> T -> Result (Result a)
rResult f js = rEither rs f js

--- wList f ls
--- Writes a 'List' which elements are 'JSON'ized with 'f'.
wList :: (a -> T) -> [a] -> T
wList fn ls = wa $ map fn ls

--- rList f js
--- Reads a 'List' created with 'wList'.
rList :: (T -> Result a) -> T -> Result [a]
rList f js = (reverse <$> ra js) >>=
             ( foldl (\r -> \e -> (f e) >>= (\x -> add x r))
               (Right []))
  where
    add :: a -> Result [a] -> Result [a]
    add x r = (\l -> x:l) <$> r

--- wMap f m
--- Writes a 'Map' which elements are 'JSON'ized with 'f'.
wMap :: (a -> T) -> Map.T a -> T
wMap f m = wList (\(k, v) -> wa [ws k, f v]) $ Map.pack m

--- rList f js
--- Reads a 'Map' created with 'wMap'.
rMap :: (T -> Result a) -> T -> Result (Map.T a)
rMap f m = rList (\js ->
  case ra js of
    Right [s, e] -> (\e1 -> \e2 -> (e1, e2)) <$> (rs s) <*> (f e)
    _ -> Left $ emsg $ "'" ++ (show m) ++ "' is not a Map"
  ) m
