{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.Rockfeller
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Hiro Kai
-- Stability   :  Experimental
-- Portability :
--
--
--
-----------------------------------------------------------------------------


--
-- For Annual Reviews journals
--

module Parser.Publisher.Rockfeller (
  rupressReader    
) where

import Parser.Import
import Text.XML.Cursor as C
import Parser.Utils


import qualified Data.Text as T

_rupressReader :: PaperReader
rupressReader :: PaperReader

_title, _journal, _volume, _pageFrom, _pageTo, _articleType, _abstract
    :: ReaderElement' (Maybe Text)

_mainHtml :: ReaderElement' (Maybe PaperMainText)
_doi :: ReaderElement' Text
_year :: ReaderElement' (Maybe Int)
_authors :: ReaderElement' [Text]
_publisher :: ReaderElement' (Maybe Text)

_rupressReader = defaultReader {
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
  refs = onlyFullL _refs
}

rupressReader
  = _rupressReader {
      readerName=(\_ -> "Rockfeller University Press")
      ,abstract=absLevel _abstract
      ,mainHtml=onlyFull _mainHtml}

_articleType _ = headm . getMeta "citation_section"

_supportedUrl _ url = boolToSupp $ any (`T.isPrefixOf` url) ["http://jcb.rupress.org/content/","http://jem.rupress.org/content/","http://jgp.rupress.org/content/"] 

_doi _ = fromMaybe "" . headm . getMeta "DC.Identifier"

_journal _ = headm . getMeta "citation_journal_title"
_publisher _ = headm .getMeta "citation_publisher"

_title _ = inner . queryT [jq| #article-title-1 |]


_volume _ = headm . getMeta "citation_volume"
_pageFrom _ = headm . getMeta "citation_firstpage"
_pageTo _ = headm . getMeta "citation_lastpage"

_year _ c = fmap (read . T.unpack) ((headm . getMeta "DC.Date") c >>= takemt 4)

_authors _ = getMeta "DC.Contributor"

_abstract _ = inner . map fromNode . removeTags ["h2"] . map node . queryT [jq| div.abstract |]
_mainHtml _ = fmap FlatHtml . inner . map fromNode . removeQueries ["ol.cit-list","div.fig","div.abstract", "span.to-top"] . map node . queryT [jq| div.section |]

_refs _ = ext . queryT [jq| #referencesTab > ul > li |]

ext = const [] -- stub

