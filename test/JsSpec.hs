-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module JsSpec (jsTest) where

import Dm.Test
import Dm.Js as Js
import qualified Dm.Map as Map

jsTest :: IO ()
jsTest = do
  let m = [("a", 1), ("b", 2)]
  putStrLn "Js test"

  teq ((rb . wb) True) (Right True)
  teq ((rb . wb) False) (Right False)
  teq ((ri . wi) 12) (Right 12)
  teq ((rd . wd) (-12.4)) (Right (-12.4))
  teq ((rs . ws) "") (Right "")
  teq ((rs . ws) "abc") (Right "abc")
  teq ((ra . wa) []) (Right [])
  teq ((ra . wa) [wi 1, ws "a"]) (Right [wi 1, ws "a"])
  teq ((ro . wo) []) (Right [])
  teq ((ro . wo) [("1", wi 1), ("2", ws "a")])
                 (Right [("1", wi 1), ("2", ws "a")])

  --putStrLn "  - Bool"
  teq (rb (wb True)) (Right True)
  let rB = fromStr "[true]"
  case rB of
    Right js -> teq (toStr js) "[true]"
    Left e -> fail e
  case rB of
    Right js ->
      case ra js of
        Right a -> teq (rb (a!!0)) (Right True)
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Int"
  teq (rd (wi 3)) (Right 3)
  let rI = fromStr "[3]"
  case rI of
    Right js -> teq (toStr js) "[3]"
    Left e -> fail e
  case rI of
    Right js ->
      case ra js of
        Right a -> teq (ri (a!!0)) (Right 3)
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Double"
  teq (rd (wd 3.2)) (Right 3.2)
  let rD = fromStr "[3.2]"
  case rD of
    Right js -> teq (toStr js) "[3.2]"
    Left e -> fail e
  case rD of
    Right js ->
      case ra js of
        Right a -> teq (rd (a!!0)) (Right 3.2)
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Empty String"
  teq (rs (ws "")) (Right "")
  let rSa = fromStr "[\"\"]"
  case rSa of
    Right js -> teq (toStr js) "[\"\"]"
    Left e -> fail e
  case rSa of
    Right js ->
      case ra js of
        Right a -> teq (rs (a!!0)) (Right "")
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Unary String"
  teq (rs (ws "a")) (Right "a")
  let rSa = fromStr "[\"a\"]"
  case rSa of
    Right js -> teq (toStr js) "[\"a\"]"
    Left e -> fail e
  case rSa of
    Right js ->
      case ra js of
        Right a -> teq (rs (a!!0)) (Right "a")
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Binary String"
  teq (rs (ws "ab")) (Right "ab")
  let rSa = fromStr "[\"ab\"]"
  case rSa of
    Right js -> teq (toStr js) "[\"ab\"]"
    Left e -> fail e
  case rSa of
    Right js ->
      case ra js of
        Right a -> teq (rs (a!!0)) (Right "ab")
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Empty Array"
  let rAa = fromStr "[]"
  case rAa of
    Right js -> teq (toStr js) "[]"
    Left e -> fail e
  case rAa of
    Right js ->
      case ra js of
        Right a -> teq (length a) 0
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Unary Array"
  let rAb = fromStr "[4]"
  case rAb of
    Right js -> teq (toStr js) "[4]"
    Left e -> fail e
  case rAb of
    Right js ->
      case ra js of
        Right a -> teq (ri (a !! 0)) (Right 4)
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Binary Array"
  let rAc = fromStr "[4, false]"
  case rAc of
    Right js -> teq (toStr js) "[4,false]"
    Left e -> fail e
  case rAc of
    Right js ->
      case ra js of
        Right a -> do
          teq (ri (a !! 0)) (Right 4)
          teq (rb (a !! 1)) (Right False)
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Empty Object"
  let rOa = fromStr "{}"
  case rOa of
    Right js -> teq (toStr js) "{}"
    Left e -> fail e
  case rOa of
    Right js ->
      case ro js of
        Right m -> teq (length m) 0
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Unary Object"
  let rOb = fromStr "{\"a\": 1}"
  case rOb of
    Right js -> teq (toStr js) "{\"a\":1}"
    Left e -> fail e
  case rOb of
    Right js ->
      case ro js of
        Right m -> case Map.get "a" m of
          Just n -> teq (ri n) (Right 1)
          Nothing -> fail "Key 'a' not found"
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Binary Object"
  let rOc = fromStr "{\"a\": 1, \"b\": false}"
  case rOc of
    Right js -> teq (toStr js) "{\"a\":1,\"b\":false}"
    Left e -> fail e
  case rOc of
    Right js ->
      case ro js of
        Right m -> do
          case Map.get "a" m of
            Just n -> teq (ri n) (Right 1)
            Nothing -> fail "Key 'a' not found"
          case Map.get "b" m of
            Just v -> teq (rb v) (Right False)
            Nothing -> fail "Key 'a' not found"
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Nothing"
  let rNothing = fromStr "[]"
  case rNothing of
    Right js -> teq (toStr js) "[]"
    Left e -> fail e
  case rNothing of
    Right js ->
      case rMaybe ri js of
        Right Nothing -> return ()
        Right _ -> fail "Fail in Nothing"
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Just"
  let rJust = fromStr "[3]"
  case rJust of
    Right js -> teq (toStr js) "[3]"
    Left e -> fail e
  case rJust of
    Right js ->
      case rMaybe ri js of
        Right Nothing -> fail "Expected 'Just 3'"
        Right (Just n) -> teq n 3
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Left"
  let rLeft = fromStr "[false, \"ab\"]"
  case rLeft of
    Right js -> teq (toStr js) "[false,\"ab\"]"
    Left e -> fail e
  case rLeft of
    Right js ->
      case rEither rs ri js of
        Right (Left n) -> teq n "ab"
        Right _ -> fail "Expected \"ab\""
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Right"
  let rRight = fromStr "[true, 5]"
  case rRight of
    Right js -> teq (toStr js) "[true,5]"
    Left e -> fail e
  case rRight of
    Right js ->
      case rEither rs ri js of
        Right (Right n) -> teq n 5
        Right _ -> fail "Expected '5'"
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Left (Result)"
  let rLeftR = fromStr "[false, \"Error\"]"
  case rLeftR of
    Right js -> teq (toStr js) "[false,\"Error\"]"
    Left e -> fail e
  case rLeftR of
    Right js ->
      case rResult ri js of
        Right n -> teq n (Left "Error")
        Left _ -> fail "Expected \"Error\""
    Left e -> fail e

  --putStrLn "  - Right (Result)"
  let rRightR = fromStr "[true, 5]"
  case rRightR of
    Right js -> teq (toStr js) "[true,5]"
    Left e -> fail e
  case rRightR of
    Right js ->
      case rResult ri js of
        Right n -> teq n (Right 5)
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Empty List"
  let rL = fromStr "[]"
  case rL of
    Right js -> teq (toStr js) "[]"
    Left e -> fail e
  case rL of
    Right js ->
      case rList ri js of
        Right ls -> teq ls []
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Unary List"
  let rL1 = fromStr "[5]"
  case rL1 of
    Right js -> teq (toStr js) "[5]"
    Left e -> fail e
  case rL1 of
    Right js ->
      case rList ri js of
        Right ls -> teq ls [5]
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Bynart List"
  let rL2 = fromStr "[5, 6]"
  case rL2 of
    Right js -> teq (toStr js) "[5,6]"
    Left e -> fail e
  case rL2 of
    Right js ->
      case rList ri js of
        Right ls -> teq ls [5, 6]
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Empty Map"
  let rM = fromStr "[]"
  case rM of
    Right js -> teq (toStr js) "[]"
    Left e -> fail e
  case rM of
    Right js ->
      case rList ri js of
        Right ls -> teq ls []
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Unary Map"
  let rM1 = fromStr "[[\"a\", 5]]"
  case rM1 of
    Right js -> teq (toStr js) "[[\"a\",5]]"
    Left e -> fail e
  case rM1 of
    Right js ->
      case rMap ri js of
        Right ls -> teq ls [("a", 5)]
        Left e -> fail e
    Left e -> fail e

  --putStrLn "  - Binary Map"
  let rM2 = fromStr "[[\"a\", 5], [\"b\", 6]]"
  case rM2 of
    Right js -> teq (toStr js) "[[\"a\",5],[\"b\",6]]"
    Left e -> fail e
  case rM2 of
    Right js ->
      case rMap ri js of
        Right ls -> teq ls [("a", 5), ("b", 6)]
        Left e -> fail e
    Left e -> fail e

  putStrLn "  Finished"
    where
      wObj (s, i) = (s, Js.wi i)
      rObj (s, js) = (s, Js.ri js)
