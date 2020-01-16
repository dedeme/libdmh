{-# LANGUAGE OverloadedStrings #-}

module CrypSpec (crypTest) where

import qualified Dm.Cryp as Cryp
import Control.Exception.Base
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as C8

crypTest :: IO ()
crypTest = do
  putStrLn "Cryp test"
  k <- Cryp.genk 6
  putStrLn $
    yes (Bs.length k == 6) ++
    yes (Cryp.key "deme" 6 == "wiWTB9") ++
    yes (Cryp.key "Generaro" 5 == "Ixy8I") ++
    yes (Cryp.key "Generara" 5 == "0DIih") ++
    yes (crDcr "01" "abc") ++
    yes (crDcr "11" "abcd") ++
    yes (crDcr "" "abc") ++
    yes (crDcr "a" "c") ++
    yes (crDcr "ab c" "xxx") ++
    yes (crDcr "\n\ta€b c" "abc") ++
    "    Finished"
  where
    yes v = assert v ""
    crDcr tx k = Cryp.decryp (Cryp.cryp tx k) k == tx
