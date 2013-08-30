{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.PNAS
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


--
-- For PNAS and journals with the similar format.
--
module Parser.Publisher.PNAS (
  pnasReader,
  jImmunolReader,
) where

import Parser.Import
import Text.XML.Cursor as C
import Parser.Utils
import Data.Text.Read
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Applicative

_pnasReader :: PaperReader
pnasReader :: PaperReader
jImmunolReader :: PaperReader

_supportedUrl :: PaperReader -> T.Text -> Maybe SupportLevel
_supportedUrl2 :: PaperReader -> T.Text -> Maybe SupportLevel

_title, _journal, _volume, _pageFrom, _pageTo, _articleType, _abstract
    :: ReaderElement' (Maybe Text)

_mainHtml :: ReaderElement' (Maybe PaperMainText)
_doi :: ReaderElement' Text
_year :: ReaderElement' (Maybe Int)
_authors :: ReaderElement' [Text]
_publisher :: ReaderElement' (Maybe Text)

_refs :: ReaderElement' [Reference]
ext :: Cursor -> Maybe Reference
cit :: Cursor -> Maybe (Maybe Citation)

_pnasReader = defaultReader {
  supportedUrl = _supportedUrl,
  doi = anyLevel _doi,
  journal = anyLevel _journal,
  publisher = anyLevel _publisher,
  title = anyLevel _title,
  volume = anyLevel _volume,
  pageFrom = anyLevel _pageFrom,
  pageTo = anyLevel _pageTo,
  year = anyLevel _year,
  authors = anyLevel _authors,
  articleType = anyLevel _articleType,
  refs = onlyFullL _refs,
  abstract=absLevel _abstract,
  mainHtml=onlyFull _mainHtml
}

pnasReader = _pnasReader {readerName=(\_ -> "PNAS")}
jImmunolReader = _pnasReader {readerName=(\_ -> "J. Immunol."),supportedUrl=_supportedUrl2}

_articleType _ _ = Nothing  -- PNAS does not seem to have any article type

_supportedUrl _ url = boolToSupp $ "http://www.pnas.org/content/" `T.isPrefixOf` url && ".full" `T.isInfixOf` url
_supportedUrl2 _ url = boolToSupp $ "http://www.jimmunol.org/content/" `T.isPrefixOf` url && ".full" `T.isInfixOf` url

_doi _ = fromMaybe "" . headm . getMeta "citation_doi"

_journal _ = headm . getMeta "citation_journal_title"
_publisher _ = headm .getMeta "citation_publisher"

_title _ c = (inner $ queryT [jq| #article-title-1 |] c) <|> ((lastMay $ queryT [jq| h1 |] c) >>= (inner . (:[])))

_volume _ = headm . getMeta "citation_volume"
_pageFrom _ = headm . getMeta "citation_firstpage"
_pageTo _ = headm . getMeta "citation_lastpage"
_year _ c = fmap (read . T.unpack) ((headm . getMeta "DC.Date") c >>= takemt 4)

_authors _ = getMeta "citation_author"

_abstract _ = inner . queryT [jq| div.abstract p |]
_mainHtml _ = fmap FlatHtml . inner . map fromNode . removeQueries qs . map node . queryT [jq| div.fulltext-view |]
  where
    qs = ["div.abstract","div.fn-group","div.license","div.ref-list",
          "div.fig","div.contributors","ul.kwd-group",
          "div.executive-summary","h1","div.table","div.section-nav"]

_refs _ = catMaybes . map ext . queryT [jq| ol.cit-list li |]

ext cur = Reference <$> rid <*> rname <*> cit cur <*> txt <*> url
  where
    rid = do
      c <- (headm . queryT [jq| a.rev-xref-ref |]) cur
      (eid . node) c
    rname = rid >>= dropmt 4
    txt = (Just . maybeText . TL.toStrict . innerText . queryT [jq| cite |]) cur
    url = Just Nothing
      

cit cur = Just $ Just $ emptyCitation{
            _citationTitle=title,_citationJournal=journal,_citationAuthors=auths,
            _citationYear=year,_citationVolume=volume,_citationPageFrom=pfrom,_citationPageTo=pto}
  where
    auths = (catMaybes . map (innert . (:[])) . queryT [jq| span.cit-auth |]) cur
    year = (f . fmap decimal . innert . queryT [jq| span.cit-pub-date |]) cur
    f :: Maybe (Either String (Int,T.Text)) -> Maybe Int
    f (Just (Right (v,_))) = Just v
    f _ = Nothing
    title = (innert . queryT [jq| span.cit-article-tite |]) cur
    journal = (innert . queryT [jq| span.cit-jnl-abbrev |]) cur
    volume = (innert . queryT [jq| span.cit-vol |]) cur
    pfrom = (innert . queryT [jq| span.cit-fpage |]) cur
    pto = (innert . queryT [jq| span.cit-lpage |]) cur
