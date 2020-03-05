-- Copyright 25-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Utilities for HTML conections between client - server

module Dm.Cgi (
  Cgi,
  klen,
  tNoExpiration,
  new,
  connect,
  authentication,
  getSessionData,
  delSession,
  addUser,
  delUser,
  getUser,
  changePass,
  changeLevel,
  get,
  ok,
  empty,
  Dm.Cgi.error,
  expired
  ) where

import Data.Int
import Data.Time.Clock.System
import Data.List
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as C8
import qualified Dm.Cryp as Cryp
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.File as File
import Dm.File((</>))

-- | @'klen'@ - Standar length of password (= 300)
klen = 300 :: Int
-- | @'tNoExpiration'@ - Maximun time of connections (= 30 days)
tNoExpiration = 2592000 :: Int -- seconds == 30 days
demeKey = concat
  [ "nkXliX8lg2kTuQSS/OoLXCk8eS4Fwmc+N7l6TTNgzM1vdKewO0cjok51vcdl",
    "OKVXyPu83xYhX6mDeDyzapxL3dIZuzwyemVw+uCNCZ01WDw82oninzp88Hef",
    "bn3pPnSMqEaP2bOdX+8yEe6sGkc3IO3e38+CqSOyDBxHCqfrZT2Sqn6SHWhR",
    "KqpJp4K96QqtVjmXwhVcST9l+u1XUPL6K9HQfEEGMGcToMGUrzNQxCzlg2g+",
    "Hg55i7iiKbA0ogENhEIFjMG+wmFDNzgjvDnNYOaPTQ7l4C8aaPsEfl3sugiw" ]



usersPath :: Cgi -> String
usersPath cgi = (home cgi) </> "users.db"

sessionsPath :: Cgi -> String
sessionsPath cgi = (home cgi) </> "sessions.db"

-- User --------------------------------------------------------------

readUsers :: Cgi -> IO [String]
readUsers cgi = do
  js <- File.read $ usersPath cgi
  return $ map Js.rs $ Js.rList $ Js.fromStr js

writeUsers :: Cgi -> [String] -> IO ()
writeUsers cgi us =
  File.write (usersPath cgi) $ Js.toStr $ Js.wList $ map Js.ws us

-- Returns (Id, key, level)
userFields :: String -> (String, String, String)
userFields l = let (user, _:rest) = span (/= ':') l
                   (key, _:level) = span (/= ':') rest
               in  (user, key, level)

removeUser :: String -> [String] -> [String]
removeUser u = filter (\l -> f $ userFields l)
  where
    f (id, _, _) = id /= u

addUser' :: Cgi -> String -> String -> String -> IO ()
addUser' cgi id key level = do
  users <- readUsers cgi
  let users' = removeUser id users
  let kkey = C8.unpack $ Cryp.key key klen
  writeUsers cgi ((id ++ ":" ++ kkey ++ ":" ++ level):users')

-- Session -----------------------------------------------------------

readSessions :: Cgi -> IO [JSValue]
readSessions cgi = do
  js <- File.read $ sessionsPath cgi
  return $ Js.rList $ Js.fromStr js

writeSessions :: Cgi -> [JSValue] -> IO ()
writeSessions cgi ss = File.write (sessionsPath cgi) $ Js.toStr $ Js.wList ss

addSession :: Cgi -> String -> String -> String -> Int -> IO ()
addSession cgi sId u k exp = do
  now <- getSystemTime
  ss <- readSessions cgi
  add now $ filter (ffilter now) ss
  where
    ffilter now sJs = let [sId', u, k, cId, tm', lapse] = Js.rList sJs
                          tm = fromIntegral (systemSeconds now) :: Int
                      in  if (sId == Js.rs sId') ||
                             (tm > Js.ri tm')
                          then False
                          else True
    add now ss = do
      let ss' = (Js.wList [Js.ws sId,
                 Js.ws u,
                 Js.ws k,
                 Js.ws "",
                 Js.wi $ fromIntegral (systemSeconds now) + exp,
                 Js.wi exp]):ss
      writeSessions cgi ss'

setConnectionId :: Cgi -> String -> String -> IO ()
setConnectionId cgi ssId conId = do
  let ssIdJs = Js.ws(ssId)
  sessions <- readSessions cgi
  case find (\js -> head (Js.rList js) == ssIdJs) sessions of
    Nothing -> return ()
    Just js -> do
      let conIdJs = Js.ws conId
      let [sId, u, k, _, tm, lapse] = Js.rList js
      let ss = filter (\js -> ffind ssIdJs (Js.rList js)) sessions
      let ss' = (Js.wList [sId, u, k, conIdJs, tm, lapse]):ss
      writeSessions cgi ss'
  where
    ffind _ [] = False
    ffind id (sId:_) = sId == id

-- Public interface --------------------------------------------------

-- | Cgi record
data Cgi = Cgi
  { key :: String -- key to encrypt communications
  , home :: String -- Application directory
  } deriving (Show)

-- | @'new' homeDir key@ - Creates a new Cgi.
--
-- > homeDir: Application directory (e.g. dmcgi/Market)
-- >     key: Key to encrypt communications
new :: String -> String -> IO Cgi
new homeDir key = do
  let cgi = Cgi {home = homeDir, key = key}
  mkHome cgi
  return cgi
  where
    mkHome cgi = do
      ex <- File.exists $ usersPath cgi
      if ex then return ()
            else do
              File.mkDir $ home cgi
              writeUsers cgi []
              addUser' cgi "admin" demeKey "0"
              writeSessions cgi []

-- | @'connect' cgi sessionId@ - Sets a new connection and returns to client:
--
-- >          key (String): Key to commnications
-- > connectionId (String): Idenfitifier for the new connection.
--
-- If connection fails, it returns {"key": "", "connectionId": ""}
connect :: Cgi -> String -> IO ()
connect cgi sessionId = do
  k <- Cryp.genk klen
  setConnectionId cgi sessionId (C8.unpack k)
  (key, conId) <- getSessionData cgi sessionId
  ok cgi [("key", Js.ws key), ("connectionId", Js.ws conId)]

-- | @'authentication' cgi user key exp@ - Authenticates /user/ and returns
--   to client:
--
-- >     level (String): User level
-- > sessionId (String): Identifier for session
-- >       key (String): Key to commnicacionts
--
-- If authentication fails, it returns {"level": "", "sessionId": "", "key":""}
authentication :: Cgi -> String -> String -> Int -> IO ()
authentication cgi user key exp = do
  let kkey = C8.unpack $ Cryp.key key klen
  u' <- getUser cgi user
  case u' of
    Nothing -> ffail
    Just (u, k, l) ->
      if k == kkey
      then do
        sIdBs <- Cryp.genk klen
        let sId = C8.unpack sIdBs
        sKeyBs <- Cryp.genk klen
        let sKey = C8.unpack sKeyBs
        addSession cgi sId u sKey exp
        ok cgi [
          ("level", Js.ws l),
          ("sessionId", Js.ws sId),
          ("key", Js.ws sKey)]
      else ffail
  where
    ffail = ok cgi [
      ("level", Js.ws ""),
      ("sessionId", Js.ws ""),
      ("key", Js.ws "")]

-- | @'getSessionData' cgi sessionId@ - Returns next data to client:
--
-- >          key (String): Key to commnications
-- > connectionId (String): Idenfitifier for the new connection.
--
-- If sessionId is unknown, it returns {"key": "", "connectionId": ""}

getSessionData :: Cgi -> String -> IO (String, String)
getSessionData cgi sessionId = do
  now' <- getSystemTime
  let now = (fromIntegral (systemSeconds now')) :: Int
  let ssIdJs = Js.ws(sessionId)
  sessions' <- readSessions cgi
  let sessions = filter (\js -> ffilter now (Js.rList js)) sessions'
  case find (\js -> ffind ssIdJs (Js.rList js)) sessions of
    Nothing -> return ("", "")
    Just js -> do
      let [sId, u, k, cId, tm, lapse] = Js.rList js
      let ss = filter (\js -> not (ffind ssIdJs (Js.rList js))) sessions
      let tm' = Js.wi $ now + Js.ri lapse
      let ss' = (Js.wList [sId, u, k, cId, tm', lapse]):ss
      writeSessions cgi ss'
      return (Js.rs k, Js.rs cId)
  where
    ffind id (sId:_) = sId == id
    ffilter now (_:_:_:_:tm:_) = Js.ri tm > now

-- | @'delSession' cgi sessionId@ - Deletes a session and returns an empty
--   response. If sessionId in unknown, it does nothing.
delSession :: Cgi -> String -> IO ()
delSession cgi sessionId = do
  let sId = Js.ws sessionId
  ss <- readSessions cgi
  writeSessions cgi $ filter (f sId) ss
  empty cgi
  where
    f sId js = let (sId':rest) = Js.rList js in sId /= sId'

-- | @'addUser' cgi admin apass user upass level@ - Add an user.
--
-- > admin: Administrator identifier
-- > apass: Administrator password
-- >  user: User identifier
-- > upass: User password
-- > level: User level
--
-- It returns to client:
--
-- > ok (Boolean): True if operation succeeded.
addUser :: Cgi -> String -> String -> String -> String -> String -> IO()
addUser cgi admin apass user upass level = do
  ad <- getUser cgi admin
  case ad of
    Nothing -> ok cgi [("ok", Js.wb False)]
    Just (_, k, l) ->
      if (l /= "0") || k /= (C8.unpack $ Cryp.key apass klen)
      then ok cgi [("ok", Js.wb False)]
      else do
        addUser' cgi user upass level
        ok cgi [("ok", Js.wb True)]

-- | @'delUser' cgi admin apass user@ - Removes an user.
--
-- > admin: Administrator identifier
-- > apass: Administrator password
-- >  user: User identifier
--
-- It returns to client:
--
-- > ok (Boolean): True if operation succeeded.
delUser :: Cgi -> String -> String -> String -> IO ()
delUser cgi admin apass user = do
  ad <- getUser cgi admin
  case ad of
    Nothing -> ok cgi [("ok", Js.wb False)]
    Just (_, k, l) ->
      if (l /= "0") || k /= (C8.unpack $ Cryp.key apass klen)
      then ok cgi [("ok", Js.wb False)]
      else do
        us <- readUsers cgi
        writeUsers cgi $ removeUser user us
        ok cgi [("ok", Js.wb True)]

-- | @'getUser' cgi id@ - Returns user data: Maybe (Id, key, level)
getUser :: Cgi -> String -> IO (Maybe (String, String, String))
getUser cgi id = do
  users <- readUsers cgi
  return $ find users
  where
    find [] = Nothing
    find (uLn:rest) = let u@(id', _, _) = userFields uLn
                      in  if id' == id then Just u
                                       else find rest

-- | @'changePass' cgi user pass newPass@ - Changes user password. It returns
--   to client:
--
-- > ok (Boolean): True if operation succeeded.
changePass :: Cgi -> String -> String -> String -> IO ()
changePass cgi user pass newPass = do
  u <- getUser cgi user
  case u of
    Nothing -> ok cgi [("ok", Js.wb False)]
    Just (_, k, l) ->
      if k == (C8.unpack $ Cryp.key pass klen)
      then do
        addUser' cgi user newPass l
        ok cgi [("ok", Js.wb True)]
      else
        ok cgi [("ok", Js.wb False)]

-- | @'changeLevel' cgi admin apass user level@ - Changes user level.
--
-- > admin: Administrator identifier
-- > apass: Administrator password
-- >  user: User identifier
-- > level: User level
--
-- It returns to client:
--
-- > ok (Boolean): True if operation succeeded.
changeLevel :: Cgi -> String -> String -> String -> String -> IO ()
changeLevel cgi admin apass user level = do
  ad <- getUser cgi admin
  case ad of
    Nothing -> ok cgi [("ok", Js.wb False)]
    Just (_, k, l) ->
      if (l /= "0") || k /= (C8.unpack $ Cryp.key apass klen)
      then ok cgi [("ok", Js.wb False)]
      else do
        u <- getUser cgi user
        case u of
          Nothing -> ok cgi [("ok", Js.wb False)]
          Just (_, k, l) -> do
            addUser' cgi user k level
            ok cgi [("ok", Js.wb True)]

-- | @'get' rq f key@ - Returns the value /key/ of a request /rq/. If /key/ is
--   unknown, it produces an error.
--
-- >>> Cgi.get rq Js.rs "rq"
-- "main"
get :: [(String, JSValue)] -> (JSValue -> a) -> String -> a
get rq f key =  case lookup key rq of
                  Nothing -> Prelude.error $ "Missing key '" ++ key ++ "'"
                  Just v -> f v

-- | @'ok' cgi rp@ - Sends a response to client. For example:
--
-- > ok cgi [
-- >         ("level", Js.ws l),
-- >         ("sessionId", Js.ws sId),
-- >         ("key", Js.ws sKey)]
ok :: Cgi -> [(String, JSValue)] -> IO ()
ok cgi rp = Bs.putStr $ Cryp.cryp (Js.toStr $ Js.wMap rp) (key cgi)

-- | @'empty' cgi@ - Sends an empty response to client. It is equals to
-- @'ok' cgi []@
empty :: Cgi -> IO ()
empty cgi = ok cgi []

-- | @'error' cgi msg@ - Sends an error response to client. It is equals to
-- @'ok' cgi [("error", Js.ws msg)]@
error :: Cgi -> String -> IO ()
error cgi msg = ok cgi [("error", Js.ws msg)]

-- | @'expired' cgi@ - Sends an expired response to client. It is equals to
-- @'ok' cgi [("expired", Js.wb True)]@
expired :: Cgi -> IO ()
expired cgi = ok cgi [("expired", Js.wb True)]

