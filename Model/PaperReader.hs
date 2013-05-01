-- PaperReaders.hs
--

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}

module Model.PaperReader where

import Import

import Data.List
import Data.Maybe
import Control.Lens
import qualified Data.Map as M
import Control.Monad(mzero)

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Data.Text.Lazy.Encoding (decodeUtf8,encodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.UTF8 ()
import Codec.Binary.UTF8.String (decodeString)
import Data.String 

import Text.Blaze.Renderer.Text
import qualified Text.HTML.DOM as H
import Text.XML.Cursor
import Text.XML (Document)
import Text.XML.Selector (maybeText)

import Data.Hashable (hash)

import Data.Aeson as Ae hiding (object) 

import Model
import Model.PaperP (renderStructured)
import Model.PaperReaderTypes

-- From paperserver-parser package
import qualified Parser.Paper as P
import Parser.PaperReader (parseHtml)

import System.IO.Error
import Control.Concurrent
import System.Process
import System.Directory (doesFileExist)

type ReaderName = String

samePaper :: Citation -> Citation -> Bool
samePaper c1 c2 = (citationDoi c1) == (citationDoi c2) || (citationUrl c1) == (citationUrl c2)

--ToDo: Complete this.
mkCitText :: Citation -> Text
mkCitText c = (fromMaybe "" $ citationJournal c) `T.append` (maybe "" (", " `T.append`) $ fmap (T.pack . show) $ citationYear c)

parsePaper :: Url -> FilePath -> IO (Maybe PaperP)
parsePaper url inpath = do
  fullhtml <- TIO.readFile inpath
  parseHtml url fullhtml   -- parseHtml is from Parser.PaperReader in paperserver-parser package

tmap2 :: (a->b,a->c) -> a -> (b,c)
tmap2 (f,g) x = (f x,g x)

-- This should not be used for parsing, currently only paperOriginalHtmlFromUrl should use this..
getHtmlFromFileUrl :: (Text -> String) -> Text -> IO Text
getHtmlFromFileUrl func url = do
  let file = func url
  estr <- try (TIO.readFile file)
  case estr of 
    Left err -> do
      TIO.putStrLn url
      print err
      return ""
    Right str -> return str


paperHtml :: PaperP -> IO Text
paperHtml p = return $ fromMaybe "" $ do
  mh <- p^.P.paperMainHtml
  return $ TL.toStrict $ renderHtml $ renderStructured mh

data PaperResourceType = RCit | RFull | RAbs | RFig | RRef | ROriginal

resourceReady :: Paper -> PaperResourceType -> IO Bool
resourceReady p typ = do
  case typ of
    RFull -> return $ isJust $ paperMainHtml p
    RAbs -> return $ isJust $ paperAbstract p
    ROriginal -> return $ T.null $ paperOriginalHtml p
    -- ToDo: check if this default "" is good or not.
    RRef -> return $ (not . null . paperReferences) p
    RFig -> return $ (not . null . paperFigures) p
    RCit -> return True -- as long as Paper exists.


type PaperP = P.Paper

--
-- ReferenceManager export
--

paperRIS :: Paper -> T.Text
paperRIS p = T.intercalate "\n" $ catMaybes [
              Just "TY - JOUR"
              , jo
              , ti
              , au
              , vo
              , sp
              , ep
              , py
              , doi
              , url
              , Just "ER - \n"
              ]
  where
    c = paperCitation p
    ti = fmap ("TI - " `T.append`) (citationTitle c)
    au = maybeText $ T.intercalate "\n" $ map ("AU - " `T.append`) (citationAuthors c)
    doi = Just $ "DO - " `T.append` (paperDoi p)
    jo = fmap ("JO - " `T.append`) (citationJournal c)
    vo = fmap ("VO - " `T.append`) (citationVolume c)
    sp = fmap ("SP - " `T.append`) (citationPageFrom c)
    ep = fmap ("EP - " `T.append`) (citationPageTo c)
    url = fmap ("UR - " `T.append`) (citationUrl c)
    py = fmap ("PY - " `T.append`) (fmap (T.pack . show) $ citationYear c)




