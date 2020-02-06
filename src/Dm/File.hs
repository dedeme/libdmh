-- Copyright 12-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | File utilities.

module Dm.File(
  ByteString,
  UTCTime,
  (</>),
  tmp,
  home,
  cwd,
  cd,
  mkDir,
  dir,
  exists,
  isDirectory,
  isLink,
  del,
  rename,
  copy,
  link,
  size,
  modified,
  appendBs,
  writeBs,
  readBs,
  append,
  write,
  Dm.File.read,
  name,
  parent,
  baseName,
  extension
  ) where

import qualified System.Directory as Dir
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString.Char8 as C8
import qualified System.FilePath.Posix as Fp
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import System.FilePath.Posix ((</>))

import qualified Dm.Cryp as Cryp

-- | @'tmp' tpl@ - Returns a new file path whose template is:
--                 @/tmp/tplxxxxxxxxxx
tmp :: String -> IO FilePath
tmp tpl = do
  k <- Cryp.genk 10
  return $ "/tmp/" ++ tpl ++ (C8.unpack k)

-- | @'home'@ - Returns the home directory.
home :: IO FilePath
home = Dir.getHomeDirectory

-- | @'cwd'@ - Returns the current directory.
cwd :: IO FilePath
cwd = do
  r <- Dir.getCurrentDirectory
  Dir.makeAbsolute r

-- | @'cd' path@ - Changes the current directory to /path/.
cd :: FilePath -> IO ()
cd = Dir.setCurrentDirectory

-- | @'mkdir' path@ - Creates a directory in /path/ if it does not exist. This
--                    function creates parent directories if is need.
mkDir :: FilePath -> IO ()
mkDir = Dir.createDirectoryIfMissing True

-- | @'dir' path@ - Returns a list of all entries in dir without the special
--                  entries (. and ..)
dir :: FilePath -> IO [String]
dir = Dir.listDirectory

-- | @'exists' path@ - Returns 'True' if /path/ exists in file system.
exists :: FilePath -> IO Bool
exists = Dir.doesPathExist

-- | @'isDirectory' path@ - Returns 'True' if /path/ exists and is a directory.
isDirectory :: FilePath -> IO Bool
isDirectory = Dir.doesDirectoryExist

-- | @'isLink' path@ - Returns 'True' if /path/ exists and is a Symlink.
isLink :: FilePath -> IO Bool
isLink path = do
  ex <- exists path
  if ex
  then Dir.pathIsSymbolicLink path
  else return False

-- | @'del' path@ - Removes /path/ from file system. If /path/ does not exist,
--                  it does not make anything
del :: FilePath -> IO ()
del = Dir.removePathForcibly

-- | @'rename' old new@ - Renames /old/ as /new/ if /new/ does not exist.
rename :: FilePath -> FilePath -> IO ()
rename old new = do
  ex <- exists new
  if ex
  then fail $ "'" ++ new ++ "' already exists"
  else Dir.renamePath old new

-- | @'rename' old new@ - Copy /old/ in /new/. If /new/ exists is overwritten.
copy :: FilePath -> FilePath -> IO ()
copy = Dir.copyFile

-- | @'link' path symbol@ - Creates a symbolic link from symbol to path
link :: FilePath -> FilePath -> IO ()
link path symbol = do
  isDir <- isDirectory path
  if isDir
  then Dir.createDirectoryLink path symbol
  else Dir.createFileLink path symbol

-- | @'size' path@ - Returns the size of /path/
size :: FilePath -> IO Integer
size = Dir.getFileSize

-- | @'modified' path@ - Returns the last modified time of /path/
modified :: FilePath -> IO UTCTime
modified = Dir.getAccessTime

-- | @'appendBs' path bs@ - Appends /bs/ to /path/
appendBs :: FilePath -> ByteString -> IO ()
appendBs = Bs.appendFile

-- | @'writeBs' path bs@ - Writes /bs/ in /path/
writeBs :: FilePath -> ByteString -> IO ()
writeBs = Bs.writeFile

-- | @'readBs' path@ - Read /path/.
readBs :: FilePath -> IO ByteString
readBs = Bs.readFile

-- | @'append' path s@ - Appends /s/ to /path/ in UTF-8
append :: FilePath -> String -> IO ()
append path = (Bs.appendFile path) . U8.fromString

-- | @'write' path s@ - Writes /s/ in /path/ in UTF-8
write :: FilePath -> String -> IO ()
write path = (Bs.writeFile path) . U8.fromString

-- | @'read' path@ - Read /path/ which was written in UTF-8.
read :: FilePath -> IO String
read path = U8.toString <$> Bs.readFile path

-- | @'name' path@ - Returns the name of /path/
name :: FilePath -> String
name = Fp.takeFileName

-- | @'parent' path@ - Returns the parent of /path/
parent :: FilePath -> FilePath
parent = Fp.takeDirectory

-- | @'baseName' path@ - Returns the base name of /path/
baseName :: FilePath -> String
baseName = Fp.takeBaseName

-- | @'extension' path@ - Returns the extension of /path/ with dot.
extension :: FilePath -> String
extension = Fp.takeExtension


