-- Copyright 28-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Extern programs

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

--- zenityMsg icon msg
--- Shows a message box. It calls:
---   `zenity --notification --window-icon='icon' --text='msg' 2>/dev/null`
--- 'icon' is one of gnome icon stock. For example: info, dialog-warning,
--- dialog-error, dialog-information, face-wink, etc.
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

--- zenityEntry title prompt
--- Reads a text using GUI. It calls:
---   `zenity --entry --title='title' --text='prompt' 2>/dev/null`
--- The return removes starting and trailing spaces.
--- If user clicks on cancel, it returns an empty string.
--- It is posible set a default text adding in promp:
---   \" --entry-text \"[text_to_add]
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

--- wget url
--- Calls "wget -q --no-cache -O - 'url'" and returns the text read.
--- If the reading fails, it returns an empty string.
wget :: String -> IO String
wget url = do
  catch
    (readCreateProcess (shell ("wget -q --no-cache -O - " ++ url)) "")
    ((\ _ -> return "") :: SomeException -> IO String)

--- pdf html target options
--- Generates a pdf file from a html text. It calls:
---   `pdfPrinter -s 'tempFile' -t 'tempFile' 'options' 2>&1`
---
---   html   : Text html
---   target : Path of the new pdf file
---   options: Options for pdfPrinter
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

--- zip source target
--- Compresses source in target. It calls:
---   `zip -q 'target' 'source' 2>&1`
--- if 'target' already exists, source will be added to it. If you require a
--- fresh target file, you have to delete it previously.
---
---   source: can be a file or directory,
---   target: Zip file. If it is a relative path, it hangs on source parent.
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

--- unzip source target
--- Uncompresses source in target, It calls:
---   `unzip -q -o 'source' -d 'target' 2>&1`
---
---   source: Zip file.
---   target: A directory. It it does not exist, it is created.
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
