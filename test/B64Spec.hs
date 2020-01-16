{-# LANGUAGE OverloadedStrings #-}

module B64Spec (b64Test) where

import qualified Dm.B64 as B64
import Control.Exception.Base
import qualified Data.ByteString as Bs

b64Test :: IO ()
b64Test = do
  let b64 = B64.encode "Cañónç䍆"
  let b640 = B64.encode ""
  let bss = Bs.pack [120..129]
  putStrLn "B64 test"
  putStrLn $
    (assert (b64 == "Q2HDscOzbsOn5I2G") "") ++
    (assert (b640 == "") "") ++
    (assert (B64.decode b64 == "Cañónç䍆") "") ++
    (assert (B64.decode b640 == "") "") ++
    (assert (B64.decodeBs (B64.encodeBs bss) == bss) "") ++
    "    Finished"
