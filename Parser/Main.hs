{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import PaperReader
import System.Environment (getArgs)
import Data.List
import Data.Maybe (isJust)
import Control.Monad
import Control.Applicative
import Import hiding (Url)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB -- hiding (putStrLn)
-- import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Data.Aeson.Encode
import Data.Aeson

import Data.Text.Encoding
import Data.Time.Clock
import System.IO
import Control.Lens

import Settings
import Formatter

getNameByUrl :: Url -> Handle -> IO () 
getNameByUrl url log = do
  let str = (maybe "null" (\r -> (readerName r) r) . readerFromUrl . T.pack) url
  TIO.putStrLn str
  TIO.hPutStrLn log $ "getNameByUrl -> " `T.append` str

getNameByHtml :: Url -> Handle -> IO ()
getNameByHtml url log = do
  str <- TIO.getContents
  let rs = readersFromHtml (T.pack url) str
  if null rs then
    TIO.hPutStrLn log "No reader found."
  else if length rs == 1 then do
    let r = head rs
    TIO.hPutStrLn log $ T.append "Reader found: " (readerName r r)
  else
    TIO.hPutStrLn log $ T.append "Multiple readers found. The first one is chosen: "
      $ T.intercalate ", " $ map (\r -> readerName r r) rs
  let mr = headMay rs
  let str = maybe "null" (\r -> readerName r r)  mr
  TIO.hPutStrLn log $ "getNameByHtml -> " `T.append` str

doParse :: [Command] -> Url -> Handle -> IO ()
doParse opt url log = do
  paper <- getPaper url log
  let
    absh = fromMaybe "" $ paper^.paperAbstract
    mainh = maybe "" showMainText $ paper^.paperMainHtml
  outputResult opt ((encode paper),absh, mainh) log

outputSingleResult opt bs log = do
  let mpath = getOutPathPlain opt
  case mpath of
    Just path -> do
      hPutStrLn log $ "outputSingleResult: path=" ++ path
      LB.writeFile path bs
    Nothing -> do
      hPutStrLn log $ "outputSingleResult: stdout"
      LB.putStrLn bs

outputSingleResult' opt txt log = do
  let mpath = getOutPathPlain opt
  case mpath of
    Just path -> do
      hPutStrLn log $ "outputSingleResult: path=" ++ path
      TLIO.writeFile path txt
    Nothing -> do
      hPutStrLn log $ "outputSingleResult: stdout"
      TLIO.putStrLn txt

outputResult :: [Command] -> (LB.ByteString,T.Text,T.Text) -> Handle -> IO ()
outputResult opt (jsonbs,abst,maint) log = do
  let mpath = getOutPath opt
  case mpath of
    Just (json,absh,mainh) -> do
      hPutStrLn log $ "outputResult: paths=" ++ show (json,absh,mainh)
      LB.writeFile json jsonbs
      TIO.writeFile absh abst
      TIO.writeFile mainh maint
    Nothing -> do
      hPutStrLn log $ "outputResult: stdout"
      LB.putStrLn jsonbs

getOutPath :: [Command] -> Maybe (FilePath,FilePath,FilePath)
getOutPath cs = g <$> find f cs
  where
    f (OutputPath _) = True
    f _ = False
    g (OutputPath path) = (path++".json",path++".abstract.html",path++".main.html")

getOutPathPlain :: [Command] -> Maybe FilePath
getOutPathPlain cs = g <$> find f cs
  where
    f (OutputPath _) = True
    f _ = False
    g (OutputPath path) = path

getMainHtml :: [Command] -> Url -> Handle -> IO ()
getMainHtml opt url log = do
  p <- getPaper url log
  let
    mpath = getOutPath opt
    maint = maybe "" showMainText $ p^.paperMainHtml
  case mpath of
    Just (_,_,mainh) -> do
      hPutStrLn log $ "getMainHtml: path=" ++ mainh
      TIO.writeFile mainh maint
    Nothing -> do
      hPutStrLn log $ "getMainHtml: stdout" 
      TIO.putStrLn maint

getPaper :: Url -> Handle -> IO Paper
getPaper url log = do
  html <- TIO.getContents -- Stub: maybe eliminate this lazy IO
  let paper = emptyPaper{_paperUrl = T.pack url, _paperHtml = html}
  reparse paper log


testJsonParse _ _ = do
  bs <- LB.getContents
  print $ (decode bs :: Maybe Paper)


type Url = String
data Command = GetNameByUrl Url | GetNameByHtml Url | ParseHtml Url |
               GetMeta Url | GetMainHtml Url | TestJSON |  
               GetFormatted Format Url |
               OutputPath FilePath | 
               NotSupported | Invalid
     deriving (Eq,Show,Ord)
parseArgs :: [String] -> [Command]
parseArgs as = sort $ parseArgs0 as []

parseArgs0 :: [String] -> [Command] -> [Command]
parseArgs0 [] as = as
parseArgs0 ("--name":"--url":url:xs) as = parseArgs0 xs ((GetNameByUrl url):as)
parseArgs0 ("--name":"--html":url:xs) as = parseArgs0 xs ((GetNameByHtml url):as)
-- parseArgs0 ("--parse-meta":"--url":url:xs) as = [NotSupported] 
-- parseArgs0 ("--parse-meta":"--html":url:xs) as = parseArgs0 xs ((GetMeta url):as)
parseArgs0 ("--parse":"--url":url:xs) as = [NotSupported] 
parseArgs0 ("--parse":"--html":url:xs) as = parseArgs0 xs ((ParseHtml url):as)
parseArgs0 ("--formatA":"--html":url:xs) as = parseArgs0 xs ((GetFormatted FormatA url):as)
parseArgs0 ("--formatB":"--html":url:xs) as = parseArgs0 xs ((GetFormatted FormatB url):as)
parseArgs0 ("--mainhtml":"--url":url:xs) as = [NotSupported]
parseArgs0 ("--mainhtml":"--html":url:xs) as = parseArgs0 xs ((GetMainHtml url):as)
parseArgs0 ("--testjson":"--html":xs) as = parseArgs0 xs (TestJSON:as)
parseArgs0 ("--out":path:xs) as = parseArgs0 xs ((OutputPath path):as)
parseArgs0 _ _ = [Invalid]

--
-- ./paper-parser --parse --html http://pubs.acs.org/doi/full/10.1021/ja310432u < test_acs.html
--
main = do
  args <- getArgs
  out <- openFile logFile AppendMode
  time <- getCurrentTime
  hPutStrLn out $ "Executed: " ++ show time
  hPutStrLn out $ "Params: " ++ show args
  let opt = parseArgs args
  hPutStrLn out $ "Command: " ++ show opt
  case headMay opt of
    Just (GetNameByUrl url) -> getNameByUrl url out
    Just (GetNameByHtml url) -> getNameByHtml url out
    Just (ParseHtml url) -> doParse opt url out
    Just TestJSON -> testJsonParse opt out
    Just (GetMainHtml url) -> getMainHtml opt url out
    Just (GetFormatted f url) -> do
      paper <- getPaper url out
      outputSingleResult opt (format f paper) out
    Just NotSupported -> putStrLn "Operation not supported yet."
    Just Invalid -> do
      putStrLn "Invalid parameters."
      print opt
      printUsage
    _ -> printUsage
  hPutStrLn out ""
  hClose out
  return ()

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: paper-parser [--name|--parse|--mainhtml|--formatA|--formatB] [--url|--html] <url for the html>"
  putStrLn "When you specify --html, stdio is used as input of html."
