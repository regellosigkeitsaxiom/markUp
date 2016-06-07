{-# LANGUAGE BangPatterns, ScopedTypeVariables, TemplateHaskell #-}
module Main where

--import Style

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Control.Concurrent
import Control.Monad ( liftM )
import System.Process
import System.Directory
import System.Environment
import System.Exit
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TT
import Control.Exception
import Data.ByteString.UTF8
import Data.FileEmbed
import Data.List ( find )
import Data.Maybe ( fromMaybe )

main :: IO ()
main = do
  initGUI
  filename <- getFileName
  style <- getStyleName
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
  x <- loadFile filename style
  webViewLoadString wv x Nothing "/"
  widgetShowAll w
  forkOS $ launchWatchdog filename style wv
  forkOS $ editor filename
  mainGUI --does not work with forkOS/forkIO

restart x = catch x ( \( e :: SomeException ) -> do
                      threadDelay 10000
                      restart x
                    )

launchWatchdog file style webview = do
  t <- getModificationTime file
  restart $ watchdog file style t webview

loadFile :: String -> String -> IO String
loadFile file style = do
  !mrkdwn <- TT.readFile file
  let qq = readMarkdown def ( useCSS style ++ T.unpack mrkdwn )
  let pp = either (error "Could not parse markDown (as if possible)") id qq
  return $ writeHtmlString def pp

markdownOpts :: ReaderOptions
markdownOpts = def
  { readerExtensions = githubMarkdownExtensions }

watchdog file style start webview = do
  t <- getModificationTime file
  if t == start
  then do
    threadDelay 100000
    restart $ watchdog file style start webview
  else do
    s <- loadFile file style
    postGUISync $ webViewLoadString webview s Nothing "/"
    threadDelay 100000
    restart $ watchdog file style t webview
    

editor :: String ->  IO ()
editor file = do
  ed <- runCommand $ "$EDITOR " ++ file
  waitForProcess ed
  mainQuit

getStyleName :: IO String
getStyleName = do
  args <- getArgs
  if args == []
  then die "Usage: markUp style file"
  else return $ head args

getFileName :: IO FilePath
getFileName = do
  args <- getArgs
  if Prelude.length args < 2
  then die "Need (.md) file as argument"
  else return $ args !! 1

useCSS s = fromMaybe style_avenirWhite ( pickStyle (s++".css"))

--Could use embedding whole directory here

style_avenirWhite = "<style>"
      ++ toString $( embedFile "styles/avenir-white.css" )
      ++ "</style>"

pickStyle :: FilePath -> Maybe String
pickStyle f = liftM ( wrap . snd ) . find ( \(a,_) -> a==f ) $ styles
  where
  wrap x = "<style>" ++ toString x ++ "</style>"

styles = $( embedDir "styles" )
