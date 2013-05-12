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

-- elsevier1Reader : Immunity: http://dx.doi.org/10.1016/j.immuni.2006.04.010
--                   Archives of Biochemistry and Biophysics:  http://dx.doi.org/10.1016/j.abb.2011.05.014
-- elsevier2Reader : Not supported yet.
--

import Parser.Import
-- import Safe
import Text.XML
import Text.XML.Cursor as C

import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import qualified Data.Text as T
import Parser.Utils

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

_doi _ = T.drop 18 . innerText . query "#ddDoi"

-- _journal _ = maybeText . innerText . queryT [jq|div.title span|]
_journal _ = (^.citationJournal) . cit

_volume _ = (^.citationVolume) . cit

_pageFrom _ = (^.citationPageFrom) . cit
_pageTo _ c = (^.citationPageTo) (trace (show (cit c)) $ cit c)
_year _ = (^.citationYear) . cit              

_authors _ = (^.citationAuthors) . cit
_articleType _ = maybeText . T.strip . innerText . queryT [jq| p.article-type |]
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
    txt = innerText $ queryT [jq| p.volIssue |] c
  in
    case parse p "" txt of
      Left err -> emptyCitation
      Right (v,pf,pt,y) ->
        emptyCitation{_citationVolume = Just v,_citationPageFrom=Just pf,
          _citationPageTo=Just pt,_citationYear=Just y}

------
parseCit :: [T.Text] -> Citation
parseCit ts | length ts == 6
              = emptyCitation
                          {_citationTitle=Just (ts!!0),_citationAuthors=T.splitOn ", " (ts!!1),
                            _citationJournal=Just (ts!!3),
                            _citationVolume=fmap vol c,_citationDoi=Just doi,_citationPageFrom=fmap pf c,
                            _citationPageTo=fmap pt c,_citationYear=fmap year c}
            | otherwise = emptyCitation
  where
    doi = T.drop 5 (ts!!5)
    c = trace (show ts) $ either (const Nothing) Just (parse p "" (ts!!4))
    vol (v,_,_,_) = v
    pf (_,p,_,_) = p
    pt (_,_,p,_) = p
    year (_,_,_,y) = y


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

