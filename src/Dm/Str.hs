-- Copyright 14-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module Dm.Str
  ( split
  , Dm.Str.break
  , breakLast
  , join
  , replace
  , starts
  , ends
  , ltrim
  , rtrim
  , trim
  , toUpper
  , toLower
  , sub
  , left
  , right
  )where

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Char as Char

--- split sep s
--- 'sep' can not be "".
split :: String -> String -> [String]
split sep s = map Text.unpack $ Text.splitOn (Text.pack sep) (Text.pack s)

--- break sep s
--- 'sep' can not be "".
break :: String -> String -> (String, String)
break sep s = let (l, r) = Text.breakOn (Text.pack sep) (Text.pack s)
              in  (Text.unpack l, Text.unpack r)

--- breakLast sep s
--- 'sep' can not be "".
breakLast :: String -> String -> (String, String)
breakLast sep s = let (l, r) = Text.breakOnEnd (Text.pack sep) (Text.pack s)
              in  (Text.unpack l, Text.unpack r)

--- join sub ss
join :: String -> [String] -> String
join sep ss = List.intercalate sep ss

--- replace sub repl s
--- 'sub' can not be "".
replace :: String -> String -> String -> String
replace sub repl s = join repl $ split sub s

--- starts sub s
starts :: String -> String -> Bool
starts sub s = Text.isPrefixOf  (Text.pack sub) (Text.pack s)

--- ends sub s
ends :: String -> String -> Bool
ends sub s = Text.isSuffixOf (Text.pack sub) (Text.pack s)

--- ltrim s
ltrim :: String -> String
ltrim = dropWhile Char.isSpace

--- rtrim s
rtrim :: String -> String
rtrim = reverse . (dropWhile Char.isSpace) . reverse

--- trim s
trim :: String -> String
trim = ltrim . rtrim

--- toUpper
toUpper :: String -> String
toUpper = Text.unpack . Text.toUpper . Text.pack

--- toLower
toLower :: String -> String
toLower = Text.unpack . Text.toLower . Text.pack

--- sub start end s
sub :: Int -> Int -> String -> String
sub start end s = drop (sig start) $ take (sig end) s
  where
    sig :: Int -> Int
    sig n = if n >= 0 then n else length s + n

--- left ix s
left :: Int -> String -> String
left ix s = let ix' = if ix >= 0 then ix else length s + ix
            in  take ix' s

--- right ix s
right :: Int -> String -> String
right ix s = let ix' = if ix >= 0 then ix else length s + ix
             in  drop ix' s
