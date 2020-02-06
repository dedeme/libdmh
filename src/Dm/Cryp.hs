-- Copyright 06-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Encryption utilities.
module Dm.Cryp
    ( genk,
      key,
      cryp,
      decryp
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Storable (pokeByteOff, peekByteOff)
import System.Random (getStdRandom, randomR)
import Data.Array((!), listArray)
import Data.Word
import qualified Dm.B64 as B64
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal (ByteString(..))

--- genk n
--- Generates a random key of /n/ b64 characters
genk :: Int -> IO(ByteString)
genk len = do
  w8 <- mapM rnd [x | x <- [1..lg]]
  return $ Bs.pack w8
  where
    rnd _ = do
      i <- getStdRandom (randomR (1, length dic))
      return $ adic ! i
    lg = if len > 0 then len else 1
    dic = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    adic = listArray (1, length dic) (Bs.unpack (C8.pack dic))

--- key s len
--- Returns 's' irreversibly encoded to a BysteString of length 'len'
key :: String -> Int -> ByteString
key k len = unsafePerformIO $ do
  let k0 = k ++ "codified in irreversibleDeme is good, very good!\n\r8@@"
  let k' = case B64.decodeBs $ B64.encode k0 of
              Left msg -> error msg
              Right v -> v
  let lenk = Bs.length k'
  let len2 = len + lenk
  let sum0 = toInt (Bs.foldl' (+) 0 k')
  ra <- mallocArray len2 :: IO (Ptr Word8)
  ra1 <- mallocArray len2 :: IO (Ptr Word8)
  ra2 <- mallocArray len2 :: IO (Ptr Word8)
  let loop1 i sm =
        if i == len2
        then return sm
        else
          let v1 = toInt (Bs.index k' (i `mod` lenk))
              v2 = v1 + (toInt $ Bs.index k' (v1 `mod` lenk))
              v3 = v2 + (toInt $ Bs.index k' (v2 `mod` lenk))
              v4 = v3 + (toInt $ Bs.index k' (v3 `mod` lenk))
              sm' = sm + i + v4
              v = toWord8 sm'
          in  do
            pokeByteOff ra1 i v
            pokeByteOff ra2 i v
            loop1 (i + 1) sm'
  sum1 <- loop1 0 sum0
  let loop2 i sm =
        if i == len
        then return ()
        else do
          v1' <- peekByteOff ra2 i :: IO Word8
          let v1 = toInt v1'
          v2' <- peekByteOff ra2 (v1 `mod` len2) :: IO Word8
          let v2 = v1 + toInt v2'
          v3' <- peekByteOff ra2 (v2 `mod` len2) :: IO Word8
          let v3 = v2 + toInt v3'
          v4' <- peekByteOff ra2 (v3 `mod` len2) :: IO Word8
          let v4 = v3 + toInt v4'
          let sm' = sm + v4
          let v = toWord8 sm'
          pokeByteOff ra2 i v
          va1 <- peekByteOff ra1 i
          pokeByteOff ra i (v + va1)
          loop2 (i + 1) sm'
  loop2 0 sum1
  rBs <- peekArray len ra
  free ra
  free ra1
  free ra2
  return $ Bs.take len (B64.encodeBs $ Bs.pack rBs)
  where
    toInt w = (fromIntegral w)::Int
    toWord8 i = (fromIntegral i)::Word8

bsMap :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
bsMap f bs1 bs2 = unsafePerformIO $ do
  let len = Bs.length bs1
  r <- mallocArray len :: IO (Ptr Word8)
  let loop i =
        if i == len
        then return ()
        else do
          pokeByteOff r i $ f (Bs.index bs1 i) (Bs.index bs2 i)
          loop (i + 1)
  loop 0
  rBs <- peekArray len r
  free r
  return $ Bs.pack rBs

--- cryp text key
--- Returns 'text' codified with 'key'
cryp :: String -> String -> ByteString
cryp text "" = error "Key is a blank string"
cryp text k =
  let text' = B64.encode text
      k' = key k (Bs.length text')
  in  B64.encodeBs $ bsMap (\x y -> x + y) text' k'

--- decryp code key
--- Returns the string which was codified with 'key' in 'code'
decryp :: ByteString -> String -> Either String String
decryp code "" = Left "Key is a blank string"
decryp code k =
  B64.decodeBs code >>= (\code' ->
    let k' = key k (Bs.length code')
    in  B64.decode $ bsMap (\x y -> x - y) code' k'
  )
