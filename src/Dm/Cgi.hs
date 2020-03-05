-- Copyright 07-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Communications protocol.

module Dm.Cgi
  ( Expiration(..)
  , T (home)
  , klen
  , new
  , authentication
  , connect
  , changePass
  , getComKey
  , endSession
  , rp
  , emptyRp
  , rrq
  ) where

import qualified Dm.Cryp as Cryp
import qualified Dm.File as File
import Dm.File ((</>))
import qualified Dm.Time as Time
import qualified Dm.Js as Js
import qualified Dm.Map as Map
import Dm.Result
import Data.List (find)

import Dm.Test

--- klen
--- Standar length of password
klen = 300 :: Int

-- tNoExpiration - Maximun time of connections (= 30 days)
tNoExpiration = 2592000000 :: Int -- milliseconds == 30 days
demeKey = concat
  [ "nkXliX8lg2kTuQSS/OoLXCk8eS4Fwmc+N7l6TTNgzM1vdKewO0cjok51vcdl",
    "OKVXyPu83xYhX6mDeDyzapxL3dIZuzwyemVw+uCNCZ01WDw82oninzp88Hef",
    "bn3pPnSMqEaP2bOdX+8yEe6sGkc3IO3e38+CqSOyDBxHCqfrZT2Sqn6SHWhR",
    "KqpJp4K96QqtVjmXwhVcST9l+u1XUPL6K9HQfEEGMGcToMGUrzNQxCzlg2g+",
    "Hg55i7iiKbA0ogENhEIFjMG+wmFDNzgjvDnNYOaPTQ7l4C8aaPsEfl3sugiw" ]
fileKey = Cryp.key demeKey klen
userDb = "users.db"
sessionDb = "sessions.db"


--- Expiration = Expire | NotExpire
--- If Expiration is NotExpire, session will be kept 30 days
data Expiration = Expire | NotExpire

--- Cgi record
data T = Cgi
  { key :: Maybe String  -- key to encrypt communications
  , home :: String       -- Application directory
  , expiration :: Int    -- Expiration delay in milliseconds
  } deriving (Show)

--- new home key exp
---   home: Path of application root.
---   key : Communications key.
---   exp : Expiration time in milliseconds
new :: String -> Maybe String -> Int -> IO T
new home key exp = dbinit >> (return $ Cgi key home exp)
  where
    dbinit = do
      x <- File.exists (home </> userDb)
      if x then return ()
      else do
       File.mkDir home
       uwrite home [User "admin" (Cryp.key demeKey klen) "0"]
       swrite home []

--- authentication cgi user pass exp
---   cgi : Connection manager.
---   user: User name.
---   pass: User password encrypted
---   exp : If its value is Expire, (expiration cgi) will be used as
---         expiration time. Otherwise, it will be used 2592000000 millisconds
---         (30 days)
authentication :: T -> String -> String -> Expiration -> IO ()
authentication cgi user pass exp = do
  let key = Cryp.key pass klen
  us <- uread $ home cgi
  case find (\(User u k _) -> user == u && key == k) us of
    Just (User _ _ l) -> do
      ss <- sread $ home cgi
      sId <- genSessionId ss
      k <- Cryp.genk klen
      now <- Time.now
      let ex = case exp of Expire -> expiration cgi; _ -> tNoExpiration
      swrite (home cgi) $ (Session sId user l k ex (Time.add ex now) : ss)
      rp cgi [("sessionId", Js.ws sId), ("key", Js.ws k)]
    _ -> rp cgi [("sessionId", Js.ws ""), ("key", Js.ws "")]


--- connect cgi sessionId
connect :: T -> String -> IO ()
connect cgi sessionId = do
  (ss, s) <- readSession (home cgi) sessionId
  (ss', u, l, k) <- case s of
               Nothing -> return (ss, "", "", "")
               Just (Session id user level key delay ex) -> do
                now <- Time.now
                if now > ex then return (ss, "", "", "")
                else
                  return ( Session id user level key
                                   delay (Time.add delay now):ss
                         , user
                         , level
                         , key
                         )
  swrite (home cgi) ss'
  rp cgi [ ("user", Js.ws u)
         , ("level", Js.ws l)
         , ("key", Js.ws k)
         ]

---
changePass :: T -> String -> String -> String -> IO ()
changePass cgi user old new = do
  us <- uread (home cgi)
  case find (\u -> user == (uid u)) us of
    Nothing -> rp cgi [("ok", Js.wb False)]
    Just u@(User _ uk _) -> do
      let old' = Cryp.key old klen
      if (uk /= old') then rp cgi [("ok", Js.wb False)]
      else do
        let new' = Cryp.key new klen
        let us' = filter (\u -> user /= (uid u)) us
        uwrite (home cgi) (u { ukey = new' }:us)
        rp cgi [("ok", Js.wb True)]

--- getComKey cgi ssId
getComKey :: T -> String -> IO (Maybe String)
getComKey cgi ssId = do
  ss <- readSession (home cgi) ssId
  case ss of
    (_, Just s) -> return $ Just (skey s)
    _ -> return Nothing

--- endSession cgi sessionId
endSession :: T -> String -> IO ()
endSession cgi sessionId = do
  ss <- sread (home cgi)
  let ss' = filter (\s -> sessionId /= (sid s)) ss
  swrite (home cgi) ss'
  emptyRp cgi

--- mkrp cgi m
rp :: T -> Map.T Js.T -> IO ()
rp cgi m = case key cgi of
             Just k -> putStrLn $ Cryp.cryp (Js.toStr $ Js.wo m) k
             _ -> fail "Cgi.Ok: Cryp key is missing"

---
emptyRp :: T -> IO ()
emptyRp cgi = rp cgi []

---
rrq :: String -> Map.T Js.T -> String -> (Js.T -> Result a) -> a
rrq source rq k jsFn =
  let msg = source ++ ": Key '" ++ k ++ "' is missing"
  in  case toResult msg (Map.get k rq) >>= jsFn of
        Right r -> r
        Left e -> error e
  where
    toResult _ (Just e) = Right e
    toResult msg _ = Left msg

-- PRIVATE

data User = User
  { uid :: String
  , ukey :: String
  , ulevel :: String
  } deriving (Show)

uwrite :: String -> [User] -> IO ()
uwrite home us = File.write (home </> userDb) $
                            Cryp.cryp (Js.toStr toJs) fileKey
  where
    toJs = Js.wList uToJs us
    uToJs (User i k l) = Js.wa [Js.ws i, Js.ws k, Js.ws l]

uread :: String -> IO [User]
uread home = do
  tx <- File.read $ home </> userDb
  case Cryp.decryp tx fileKey >>= Js.fromStr >>= (Js.rList uFromJs) of
    Right u -> return u
    Left e -> fail e
  where
    uFromJs js = Js.ra js >>=
      \a -> User <$> Js.rs (a!!0) <*> Js.rs (a!!1) <*> Js.rs (a!!2)

ureadu :: String -> String -> IO (Maybe User)
ureadu home id = do
  us <- uread home
  return $ find (\u -> id == (uid u)) us

data Session = Session
  { sid :: String
  , suser :: String
  , slevel :: String
  , skey :: String
  , sdelay :: Int
  , sex :: Time.T
  }

swrite :: String -> [Session] -> IO ()
swrite home ss =
  File.write (home </> sessionDb) $ Cryp.cryp (Js.toStr toJs) fileKey
  where
    toJs = Js.wList sToJs ss
    sToJs (Session i u l k d e) =
      Js.wa [Js.ws i, Js.ws u, Js.ws l, Js.ws k, Js.wi d, Time.toJs e]

sread :: String -> IO [Session]
sread home = do
  tx <- File.read $ home </> sessionDb
  case Cryp.decryp tx fileKey >>= Js.fromStr >>= (Js.rList sFromJs) of
    Right s -> return s
    Left e -> fail e
  where
    sFromJs js = Js.ra js >>=
      \a -> Session <$> Js.rs (a!!0) <*>
                        Js.rs (a!!1) <*> Js.rs (a!!2) <*> Js.rs (a!!3) <*>
                        Js.ri (a!!4) <*> Time.fromJs (a!!5)

genSessionId :: [Session] -> IO String
genSessionId ss = do
  id <- Cryp.genk klen
  case find (\s -> (sid s) == id) ss of
    Just _ -> genSessionId ss
    _ -> return id

-- Returns [Session] without 'session'
readSession :: String -> String -> IO ([Session], Maybe Session)
readSession home sessionId = do
  ss <- sread home
  case find (\s -> sessionId == (sid s)) ss of
    Nothing -> return (ss, Nothing)
    Just s -> do
      let ss' = filter (\s' -> sessionId /= (sid s)) ss
      swrite home (s { sex = Time.add (sdelay s) (sex s) }:ss')
      return (ss', Just s)
