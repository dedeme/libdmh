module JsSpec (jsTest) where

import qualified Dm.Js as Js
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

jsTest :: IO ()
jsTest = do
  let m = [("a", 1), ("b", 2)]
  putStrLn "Js test"
  putStrLn $ alist [
    (assert, (Js.rBool . Js.wBool) True),
    (assert, not $ (Js.rBool . Js.wBool) False),
    (assert, (Js.rInt . Js.wInt) 12 == 12),
    (assert, (Js.rDouble . Js.wDouble) (-12.4) == (-12.4)),
    (assert, (Js.rString . Js.wString) "" == ""),
    (assert, (Js.rString . Js.wString) "abc" == "abc"),
    (assert, (Js.rList . Js.wList) [] == []),
    (assert,
      map Js.rInt ((Js.rList . Js.wList) (map Js.wInt [1, 2])) == [1, 2]),
    (assert, (Js.rMap . Js.wMap) [] == []),
    (assert,
      map rObj ((Js.rMap . Js.wMap) (map wObj m)) == m),
    (assert, (Js.rMaybe . Js.wMaybe) Nothing == Nothing),
    (assert,
      (Js.rMaybe . Js.wMaybe) (Just (Js.wInt 34)) == (Just (Js.wInt 34))),
    (assert,
      (Js.rEither . Js.wEither) (Left (Js.wInt 34)) == (Left (Js.wInt 34))),
    (assert,
      (Js.rEither . Js.wEither) (Right (Js.wInt 34)) == (Right (Js.wInt 34)))
    ] ++
    "    Finished"
    where
      wObj (s, i) = (s, Js.wInt i)
      rObj (s, js) = (s, Js.rInt js)
