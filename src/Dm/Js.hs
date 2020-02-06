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
import Dm.Map as Map
import Dm.Either (Result)

---
type T = JSValue

emsg :: String -> String
emsg s = "JSON error.\n" ++ s

--- toStr js
--- Returns a string representing 'js'. 'js' must be a List or a Map.
toStr :: JSValue -> String
toStr = encodeStrict

--- fromStr s
--- Returns the JSON object corresponding to 's'.
fromStr :: String -> Result JSValue
fromStr js = case decodeStrict js of
  Ok v -> Right v
  Error e -> Left e

--- wb v
-- Returns the JSON value of 'v'.
wb :: Bool -> JSValue
wb = showJSON

--- wi v
--- Returns the JSON value of 'v'.
wi :: Int -> JSValue
wi = showJSON

--- wd v
--- Returns the JSON value of 'v'.
wd :: Double -> JSValue
wd = showJSON

--- ws v
--- Returns the JSON value of 'v'.
ws :: String -> JSValue
ws = showJSON

--- wa v
--- Returns the JSON value of 'v'.
wa :: [JSValue] -> JSValue
wa = showJSONs

--- wo v
--- Returns the JSON value of 'v'.
wo :: Map JSValue -> JSValue
wo ls = JSObject $ toJSObject ls

--- rb js
--- Retuns the value of 'js'.
rb :: JSValue -> Result Bool
rb (JSBool v) = Right v
rb js = Left $ emsg $ "'" ++ (show js) ++ "' is not a Bool"

--- ri js
--- Retuns the value of 'js'.
ri :: JSValue -> Result Int
ri (JSRational _ v) = Right $ truncate v
ri js = Left $ emsg $ "'" ++ (show js) ++ "' is not an Int"

--- rd js
--- Retuns the value of 'js'.
rd :: JSValue -> Result Double
rd (JSRational _ v) = Right $ fromRational v
rd js = Left $ emsg $ "'" ++ (show js) ++ "' is not a Double"

--- rs js
--- Retuns the value of 'js'.
rs :: JSValue -> Result String
rs (JSString v) = Right $ fromJSString v
rs js = Left $ emsg $ "'" ++ (show js) ++ "' is not a String"

--- ra js
--- Retuns the value of 'js'.
ra :: JSValue -> Result [JSValue]
ra (JSArray v) = Right v
ra js = Left $ emsg$ "'" ++ (show js) ++ "' is not an Array"

--- ro js
--- Retuns the value of 'js'.
ro :: JSValue -> Result (Map JSValue)
ro (JSObject o) = Right $ fromJSObject o
ro js = Left $ emsg "'" ++ (show js) ++ "' is not an Object"

-- DERIVATES -------------------------------------------------------------------

--- wMaybe f v
--- Writes a Maybe value.
wMaybe :: (a -> JSValue) -> Maybe a -> JSValue
wMaybe f (Just v) = wa [f v]
wMaybe _ Nothing = wa []

--- rMaybe f js
--- Reads a Maybe value.
rMaybe :: (JSValue -> Result a) -> JSValue -> Result (Maybe a)
rMaybe f js = case ra js of
              Right [] -> Right Nothing
              Right [j] -> f j >>= (Right . Just)
              _ -> Left $ emsg $ "'" ++ (show js) ++ "' is not an Maybe"

--- wEither fLeft fRight v
--- Writes a Either value.
wEither :: (a -> JSValue) -> (b -> JSValue) -> Either a b -> JSValue
wEither f _ (Left a) = wa [wb False, f a]
wEither _ f (Right b) = wa [wb True, f b]

--- rEither fLeft fRight js
--- Reads a Either value.
rEither :: (JSValue -> Result a) -> (JSValue -> Result b) -> JSValue ->
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
wResult :: (a -> JSValue) -> Result a -> JSValue
wResult f v = wEither ws f v

--- rResult f js
--- Reads a Maybe value.
rResult :: (JSValue -> Result a) -> JSValue -> Result (Result a)
rResult f js = rEither rs f js

--- wList f ls
--- Writes a 'List' which elements are 'JSON'ized with 'f'.
wList :: (a -> JSValue) -> [a] -> JSValue
wList fn ls = wa $ map fn ls

--- rList f js
--- Reads a 'List' created with 'wList'.
rList :: (JSValue -> Result a) -> JSValue -> Result [a]
rList f js = (reverse <$> ra js) >>=
             ( foldl (\r -> \e -> (f e) >>= (\x -> add x r))
               (Right []))
  where
    add :: a -> Result [a] -> Result [a]
    add x r = (\l -> x:l) <$> r

--- wMap f m
--- Writes a 'Map' which elements are 'JSON'ized with 'f'.
wMap :: (a -> JSValue) -> Map a -> JSValue
wMap f m = wList (\(k, v) -> wa [ws k, f v]) $ Map.pack m

--- rList f js
--- Reads a 'Map' created with 'wMap'.
rMap :: (JSValue -> Result a) -> JSValue -> Result (Map a)
rMap f m = rList (\js ->
  case ra js of
    Right [s, e] -> (\e1 -> \e2 -> (e1, e2)) <$> (rs s) <*> (f e)
    _ -> Left $ emsg $ "'" ++ (show m) ++ "' is not a Map"
  ) m
