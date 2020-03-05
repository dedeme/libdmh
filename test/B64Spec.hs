-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module B64Spec (b64Test) where

import qualified Dm.B64 as B64
import qualified Data.ByteString as Bs

import Dm.Test

decode bs = case B64.decode bs of Left e -> error e; Right v -> v
decodeBs bs = case B64.decodeBs bs of Left e -> error e; Right v -> v

b64Test :: IO ()
b64Test = do
  let b64 = B64.encode "Cañónç䍆"
  let b640 = B64.encode ""
  let bss = Bs.pack [120..129]
  putStrLn "B64 test"

  teq b64 "Q2HDscOzbsOn5I2G"
  teq b640 ""
  teq (decode b64) "Cañónç䍆"
  teq (decode b640) ""
  teq (decodeBs (B64.encodeBs bss)) bss

  putStrLn "  Finished"
