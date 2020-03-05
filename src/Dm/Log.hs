-- Copyright 06-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Register of warnings and fails

module Dm.Log
  ( T
  , Type
  , Entry(..)
  , new
  , warning
  , Dm.Log.error
  , get
  ) where

import qualified Dm.Time as Time
import qualified Dm.File as File
import qualified Dm.Js as Js

data T = T
          { path :: String
          , maxEntries :: Int
          }

data Type = Warning | Error

data Entry = Entry
  { etype :: Type
  , time :: Time.T
  , msg :: String
  }

--- new path maxEntries
--- Creates a new log using file in 'path'. If 'path' does not exists, it is
--- created.
new :: String -> Int -> IO T
new path maxEntries = do
  ex <- File.exists path
  if ex then return $ T path maxEntries
  else do
    File.write path (Js.toStr $ Js.wa [])
    return $ T path maxEntries

lread :: T -> IO [Entry]
lread log = do
  tx <- File.read (path log)
  case Js.fromStr tx >>= (Js.rList lFromJs) of
    Right msg -> return msg
    _ -> fail "Log.lread: Result is Left"
  where
    lFromJs js = Js.ra js >>=
      \a -> Entry <$> tFromJs (a!!0) <*> Time.fromJs (a!!1) <*> Js.rs (a!!2)
    tFromJs js = (\v -> if v then Warning else Error) <$> Js.rb js

lwrite :: T -> [Entry] -> IO ()
lwrite log entries = File.write (path log) $ Js.toStr toJs
  where
    toJs = Js.wList lToJs entries
    lToJs (Entry tp tm m) = Js.wa [tToJs tp, Time.toJs tm, Js.ws m]
    tToJs v = Js.wb $ case v of Warning -> True; _ -> False

--- warning log msg
--- Annotates a warning message.
warning :: T -> String -> IO ()
warning log msg = do
  now <- Time.now
  l <- lread log
  lwrite log $ take (maxEntries log) ((Entry Warning now msg):l)

--- error log msg
--- Annotates a fail message.
error :: T -> String -> IO ()
error log msg = do
  now <- Time.now
  l <- lread log
  lwrite log $ take (maxEntries log) ((Entry Error now msg):l)

--- get log
--- Returns a list with 'log' entries, from after to before.
get :: T -> IO [Entry]
get = lread
