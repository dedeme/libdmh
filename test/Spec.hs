import B64Spec
import CrypSpec
import JsSpec
import RndSpec
import DateSpec
import TimeSpec

main :: IO ()
main = do
  b64Test
  crypTest
  jsTest
  rndTest
  dateTest
  timeTest
