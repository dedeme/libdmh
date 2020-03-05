-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module CrypSpec (crypTest) where

import qualified Dm.Cryp as Cryp
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as C8

import Dm.Test

decryp code k = case Cryp.decryp code k of Left m -> error m; Right v -> v

crypTest :: IO ()
crypTest = do
  putStrLn "Cryp test"

  k <- Cryp.genk 6
  teq (length k) 6
  teq (Cryp.key "deme" 6) "wiWTB9"
  teq (Cryp.key "Generaro" 5) "Ixy8I"
  teq (Cryp.key "Generara" 5) "0DIih"
  teq (Cryp.cryp "01" "abc") "s7t0bQ=="
  teq (Cryp.cryp "Cañón€%ç" "deme") "v12ftuzYeq2Xz7q7tLe8tNnHtqY="
  tyes $ crDcr "01" "abc"
  tyes $ crDcr "11" "abcd"
  tyes $ crDcr "" "abc"
  tyes $ crDcr "a" "c"
  tyes $ crDcr "ab c" "xxx"
  tyes $ crDcr "\n\ta€b c" "abc"

  putStrLn "  Finished"
  where
    crDcr tx k = decryp (Cryp.cryp tx k) k == tx
