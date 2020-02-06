-- Copyright 05-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

---  Encode and decode in B64.

module Dm.B64
    ( encode,
      decode,
      encodeBs,
      decodeBs
    ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString)
import Dm.Either (Result)

--- encode s
--- Encode an UTF-8 string in Base64
encode :: String -> ByteString
encode = B64.encode . U8.fromString

--- encodeBs bs
--- Encode a ByteString in Base64
encodeBs :: ByteString -> ByteString
encodeBs = B64.encode

--- decode bs
--- Decode a ByteString codifed in Base64 to a UTF-8 string.
decode :: ByteString -> Result String
decode bs = fmap U8.toString $ B64.decode bs

--- decodeBs bs
--- Decode a ByteString codifed in Base64.
decodeBs :: ByteString -> Result ByteString
decodeBs = B64.decode
