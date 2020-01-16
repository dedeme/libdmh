-- Copyright 07-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | JSON module.

module Dm.Js
  ( JSValue,
    toStr,
    fromStr,
    wBool,
    wInt,
    wDouble,
    wString,
    wList,
    wMap,
    wMaybe,
    wEither,
    rBool,
    rInt,
    rDouble,
    rString,
    rList,
    rMap,
    rMaybe,
    rEither
  ) where

import Text.JSON

-- | @'toStr' js@ - Returns a string representing /js/. /js/ must be a List
--                  or a Map.
toStr :: JSValue -> String
toStr = encodeStrict

-- | @'fromStr' s@ - Returns the JSON object corresponding to /s/.
fromStr :: String -> JSValue
fromStr js = case decodeStrict js of
  Ok v -> v
  Error e -> error ("Bad JSON: " ++ js)

-- | @'wBool' v@ - Returns the JSON value of /v/.
wBool :: Bool -> JSValue
wBool = showJSON

-- | @'wInt' v@ - Returns the JSON value of /v/.
wInt :: Int -> JSValue
wInt = showJSON

-- | @'wDouble' v@ - Returns the JSON value of /v/.
wDouble :: Double -> JSValue
wDouble = showJSON

-- | @'wString' v@ - Returns the JSON value of /v/.
wString :: String -> JSValue
wString = showJSON

-- | @'wList' v@ - Returns the JSON value of /v/.
wList :: [JSValue] -> JSValue
wList = showJSONs

-- | @'wMap' v@ - Returns the JSON value of /v/.
wMap :: [(String, JSValue)] -> JSValue
wMap ls = JSObject $ toJSObject ls

-- | @'wMaybe' v@ - Returns the JSON value of /v/.
wMaybe :: Maybe JSValue -> JSValue
wMaybe Nothing = JSNull
wMaybe (Just v) = v

-- | @'wEither' v@ - Returns the JSON value of /v/.
wEither :: Either JSValue JSValue -> JSValue
wEither (Left v) = wList[v, JSNull]
wEither (Right v) = wList[JSNull, v]

-- | @'rBool' js@ - Retuns the value of /js/.
rBool :: JSValue -> Bool
rBool (JSBool v) = v
rBool _ = error "Bad JSON Bool"

-- | @'rInt' js@ - Retuns the value of /js/.
rInt :: JSValue -> Int
rInt (JSRational _ v) = truncate v
rInt _ = error "Bad JSON Int"

-- | @'rDouble' js@ - Retuns the value of /js/.
rDouble :: JSValue -> Double
rDouble (JSRational _ v) = fromRational v
rDouble _ = error "Bad JSON Double"

-- | @'rString' js@ - Retuns the value of /js/.
rString :: JSValue -> String
rString (JSString v) = fromJSString v
rString _ = error "Bad JSON String"

-- | @'rList' js@ - Retuns the value of /js/.
rList :: JSValue -> [JSValue]
rList (JSArray v) = v
rList _ = error "Bad JSON List"

-- | @'rMap' js@ - Retuns the value of /js/.
rMap :: JSValue -> [(String, JSValue)]
rMap (JSObject o) = fromJSObject o
rMap _ = error "Bad JSON Map"

-- | @'rMaybe' js@ - Retuns the value of /js/.
rMaybe :: JSValue -> Maybe JSValue
rMaybe JSNull = Nothing
rMaybe v = Just v

-- | @'rEither' js@ - Retuns the value of /js/.
rEither :: JSValue -> Either JSValue JSValue
rEither js = case rList js of
  [v, JSNull] -> Left v
  [JSNull, v] -> Right v
  _ -> error "Bad JSON Either"
