-- Copyright 23-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Immutable map.
--- This object is an unsorted list of (String, a). When a old key is
--- reassigned, a new entry is added in a new 'Map'.
--- Functions 'pack' and 'delete' also creates new 'Map's.

module Dm.Map
  ( Map
  , get
  , put
  , delete
  , pack
  , keys
  , values
  ) where

--- Definition
type Map a = [(String, a)]

--- get key map
--- Gets a value from its key
get :: String -> Map a -> Maybe a
get = lookup

--- put key value m
--- Adds a pair key-value. If key already exists, the former one is not deleted
--- until calling 'pack', 'keys' or 'values'
put :: String -> a -> Map a -> Map a
put key value m = (key, value) : m

--- delete key m
--- Removes key
delete :: String -> Map a -> Map a
delete key m = filter (\(k, _) -> k /= key) m

--- pack m
--- Removes former duplicate keys.
pack :: Map a -> Map a
pack m = pack' [] m
  where
  pack' r [] = r
  pack' r (e@(k, _):rest) =
    if any (\(kr, _) -> kr == k) r then pack' r rest
                                   else pack' (e:r) rest

--- keys m
--- Returns keys of 'm'
keys :: Map a -> [String]
keys = map (\(k, _) -> k) . pack

--- values m
--- Returns values of 'm'
values :: Map a -> [a]
values = map (\(_, v) -> v) . pack
