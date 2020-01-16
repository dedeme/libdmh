-- Copyright 28-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Extern programs

module Dm.Ext (
  zenityMsg,
  zenityEntry,
  wget,
  pdf,
  Dm.Ext.zip,
  Dm.Ext.unzip
  ) where

import Text.Printf
import System.Process
import System.Exit
import Control.Exception
import Dm.File as File

zenityMsg :: String -> String -> IO ()
zenityMsg icon msg = do
  let cmd = "zenity"
  let args = ["--info",
              "--icon-name=" ++ icon ++"",
              "--text=" ++ msg ++ ""]
  (exitCode, _, stdError) <- readProcessWithExitCode cmd args ""
  if exitCode == ExitSuccess
  then return ()
  else error stdError

zenityEntry :: String -> String -> IO String
zenityEntry title prompt = do
  let cmd = "zenity"
  let args = ["--entry",
              "--title=" ++ title ++"",
              "--text=" ++ prompt ++ ""]
  (exitCode, stdOut, stdError) <- readProcessWithExitCode cmd args ""
  if exitCode == ExitSuccess
  then return (init stdOut)
  else error stdError

wget :: String -> IO String
wget url = do
  catch
    (readCreateProcess (shell ("wget -q --no-cache -O - " ++ url)) "")
    ((\ _ -> return "") :: SomeException -> IO String)

pdf :: String -> String -> String -> IO ()
pdf html target options = do
  tsource <- File.tmp "libdmh-Ext-Pdf"
  ttarget <- File.tmp "libdmh-Ext-Pdf"
  File.write tsource html
  ex <- File.exists tsource
  let cmd = "pdfPrinter"
  let args = ["-s " ++ tsource,
              "-t " ++ ttarget,
              options]
  (exitCode, _, stdError) <- readProcessWithExitCode cmd args ""
  if exitCode == ExitSuccess
  then do
    File.del tsource
    ex <- File.exists ttarget
    if ex
    then  do
      File.copy ttarget target
      File.del ttarget
    else error "Fail running pdfPrinter"
  else error stdError

zip :: String -> String -> IO ()
zip source target = do
  cd <- File.cwd
  let t = if head target == '/' then target else printf "%s/%s" cd target
  let parent = File.parent source
  let name = File.name source
  File.cd parent
  let cmd = "zip"
  let args' = ["-q", t, name]
  isDir <- File.isDirectory name
  let args = if isDir then "-r":args' else args'
  (exitCode, _, stdError) <- readProcessWithExitCode cmd args ""
  File.cd cd
  if exitCode == ExitSuccess
  then do
    ex <- File.exists t
    if ex
    then return ()
    else error "Fail running zip"
  else error stdError

unzip :: String -> String -> IO ()
unzip source target = do
  isDir <- File.isDirectory target
  if isDir
  then do
    let cmd = "unzip"
    let args = ["-q", "-o", source, "-d", target]
    (exitCode, _, stdError) <- readProcessWithExitCode cmd args ""
    if exitCode == ExitSuccess
    then do
      ex <- File.exists target
      if ex
      then return ()
      else error "Fail running unzip"
    else error stdError
  else error $ printf "'%s' is not a directory" target
