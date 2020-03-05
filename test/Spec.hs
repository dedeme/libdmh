-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

import StrSpec
import B64Spec
import CrypSpec
import JsSpec
import RndSpec
import DateSpec
import TimeSpec

main :: IO ()
main = do
  strTest
  b64Test
  crypTest
  jsTest
  rndTest
  dateTest
  timeTest
