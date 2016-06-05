{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Main where

import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Control.Concurrent
import System.Process
import System.Directory
import System.Environment
import System.Exit
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TT
import Control.Exception

main :: IO ()
main = do
  print =<< initGUI
  filename <- getFileName
  que <- doesFileExist filename
  if que
  then return ()
  else TT.writeFile filename (T.pack "")
  w <- windowNew
  sw <- scrolledWindowNew Nothing Nothing
  wv <- webViewNew
  set w
    [ containerChild := sw
    , windowDefaultWidth := 500
    , windowDefaultHeight := 500
    , containerBorderWidth := 2
    ]
  set sw [ containerChild := wv ]
  x <- loadFile filename
  webViewLoadString wv x Nothing "/"
  widgetShowAll w
  forkOS $ launchWatchdog filename wv
  forkOS $ editor filename
  mainGUI --does not work with forkOS/forkIO

restart x = catch x ( \( e :: SomeException ) -> do
                      threadDelay 10000
                      restart x
                    )

launchWatchdog file webview = do
  t <- getModificationTime file
  restart $ watchdog file t webview

loadFile :: String -> IO String
loadFile file = do
  !mrkdwn <- TT.readFile file
  let qq = readMarkdown def ( T.unpack mrkdwn )
  let pp = either (error "FUBAR") id qq
  return $ writeHtmlString def pp

markdownOpts :: ReaderOptions
markdownOpts = def
  { readerExtensions = githubMarkdownExtensions }

watchdog file start webview = do
  t <- getModificationTime file
  if t == start
  then do
    threadDelay 100000
    restart $ watchdog file start webview
  else do
    s <- loadFile file
    postGUISync $ webViewLoadString webview s Nothing "/"
    threadDelay 100000
    restart $ watchdog file t webview
    

editor :: String ->  IO ()
editor file = do
  ed <- runCommand $ "$EDITOR " ++ file
  waitForProcess ed
  mainQuit

getFileName :: IO FilePath
getFileName = do
  args <- getArgs
  if args == []
  then die "Need (.md) file as argument"
  else return $ head args
