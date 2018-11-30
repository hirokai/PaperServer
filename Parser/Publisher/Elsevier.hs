{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.Elsevier
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Hiro Kai
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parser.Publisher.Elsevier (
    elsevier1Reader,elsevier2Reader
) where

-- elsevier1Reader : Immunity: https://doi.org/10.1016/j.immuni.2006.04.010
--                   Archives of Biochemistry and Biophysics:  https://doi.org/10.1016/j.abb.2011.05.014
-- elsevier2Reader : Not supported yet.
--

import Parser.Import
import Text.XML
import Text.XML.Cursor as C

import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Parser.Utils
import qualified Parser.Lens as L
import Control.Lens


_elsevierReader = defaultReader {
  supportedUrl = _supportedUrl,
--  supported = _supported,
  title = anyLevel _title,
  abstract = anyLevel _abstract,
  doi = anyLevel _doi,
  journal = anyLevel _journal,
  volume = anyLevel _volume,
  pageFrom = anyLevel _pageFrom,
  pageTo = anyLevel _pageTo,
  year = anyLevel _year,
  authors = anyLevel _authors,
  articleType = anyLevel _articleType,
  refs = onlyFullL _refs,
  figs = onlyFullL _figs,
--  toc = _toc,
  publisher = anyLevel _publisher,
  mainHtml = onlyFull _mainHtml
--  readerName = _readerName
}

_elsevierReader, elsevier1Reader, elsevier2Reader :: PaperReader

_supportedUrl :: PaperReader -> T.Text -> Maybe SupportLevel
_sup1 :: PaperReader -> T.Text -> t -> Maybe SupportLevel
_sup2 :: PaperReader -> t1 -> t2 -> Maybe a

_title, _journal, _volume, _pageFrom, _pageTo, _articleType, _abstract
    :: ReaderElement' (Maybe Text)

_mainHtml :: ReaderElement' (Maybe PaperMainText)
_doi :: ReaderElement' Text
_year :: ReaderElement' (Maybe Int)
_authors :: ReaderElement' [Text]
_publisher :: ReaderElement' (Maybe Text)

_refs :: ReaderElement' [Reference]
_figs :: ReaderElement' [Figure]
cit2 :: Cursor -> Citation

elsevier1Reader = _elsevierReader{supported=_sup1,readerName = \x -> "Elsevier1"}
elsevier2Reader = _elsevierReader{supported=_sup2,readerName = \x -> "Elsevier2"}

_supportedUrl _ url = 
  if "http://www.sciencedirect.com/" `T.isPrefixOf` url &&
     "/article/" `T.isInfixOf` url then
    Just SAbstract
  else
    Nothing
 
_sup1 r url doc = _supportedUrl r url
_sup2 _ utl doc = Nothing  --Stub!

_title _ = inner . queryT [jq| h1.svTitle |]

_mainHtml _ = fmap FlatHtml . render . removeTags ["script"] . noFig . map node . queryT [jq|.svArticle |]
_abstract _ c = (inner . queryT [jq|#first-paragraph|]) c <|> (inner . queryT [jq|div.svAbstract p|]) c

_doi _ = T.drop 18 . TL.toStrict . innerText . query "#ddDoi"

-- _journal _ = maybeText . innerText . queryT [jq|div.title span|]
_journal _ = (^.L.journal) . cit

_volume _ = (^.L.volume) . cit

_pageFrom _ = (^.L.pageFrom) . cit
_pageTo _ c = (^.L.pageTo) (trace (show (cit c)) $ cit c)
_year _ = (^.L.year) . cit              

_authors _ = (^.L.authors) . cit
_articleType _ = maybeText . TL.toStrict . TL.strip . innerText . queryT [jq| p.article-type |]
_publisher _ _ = Just "Elsevier"
_refs _ _ = []
_figs _ _ = []

noFig :: [Node] -> [Node]
noFig = id -- Stub

citTxt :: Cursor -> [T.Text]
citTxt c = (fromMaybe [] . fmap f . headMay . queryT [jq| #citationInfo > input |]) c
  where
    f ::Cursor -> [T.Text]
    f = T.lines . T.concat . attribute "value"

cit :: Cursor -> Citation
cit c = case citTxt c of
          [] -> cit2 c
          ls -> parseCit ls

cit2 c =
  let
    txt = TL.toStrict $ innerText $ queryT [jq| p.volIssue |] c
  in
    case parse p "" txt of
      Left err -> def
      Right (v,pf,pt,y) ->
        def & L.volume .~ Just v
            & L.pageFrom .~ Just pf
            & L.pageTo .~ Just pt
            & L.year .~ Just y

------
parseCit :: [T.Text] -> Citation
parseCit ts | length ts == 6
              = (def :: Citation)
                  & L.title .~ Just (ts!!0)
                  & L.authors .~ T.splitOn ", " (ts!!1)
                  & L.journal .~ Just (ts!!3)
                  & L.volume .~ fmap (^._1) c
                  & L.doi .~ Just (T.drop 5 (ts!!5))
                  & L.pageFrom .~ fmap (^._2) c
                  & L.pageTo .~ fmap (^._3) c
                  & L.year .~ fmap (^._4) c
            | otherwise = def :: Citation
  where
    c = either (const Nothing) Just (parse p "" (ts!!4))


p :: Parser (T.Text,T.Text,T.Text,Int)
p = do
  v <- volume
  sep
  _ <- issue
  sep
  (_,_,y) <- date
  sep
--  (pf,pt) <- return ("0","1")
  (pf,pt) <- pages
  return (T.pack v,T.pack pf,T.pack pt,read y)
  
  where
    sep = string ", "
    volume = do
      string "Volume "
      v <- many1 alphaNum
      return v
    issue = do
      choice . map (try . string) $ ["Issue ", "Issues "]
      i <- many1 alphaNum
      optionMaybe (char '\8211')
      i2 <- optionMaybe (many1 alphaNum)
      return (i,i2)
    date = do
      d <- optionMaybe day
      m <- month
      char ' '
      y <- Text.Parsec.count 4 digit
      return (d,m,y)
    day = do
      d <- many1 digit
      char ' '
    pages = do
      string "Pages "
      pf <- many1 alphaNum
      char '-'
      pt <- many1 alphaNum
      return (pf,pt)
    month :: Parser String
    month = choice (map (try . string) ["January","February","March","April","May","June","July","August","September","October","November","December"])

