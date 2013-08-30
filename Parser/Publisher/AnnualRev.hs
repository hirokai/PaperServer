{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.AnnualRev
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

module Parser.Publisher.AnnualRev (
  annualRevReader    
) where

import Parser.Import
import Text.XML.Cursor as C
import Parser.Utils
import qualified Data.Text as T
import Control.Applicative

_annualRevReader :: PaperReader
annualRevReader :: PaperReader

_supportedUrl :: PaperReader -> T.Text -> Maybe SupportLevel

_title, _journal, _volume, _pageFrom, _pageTo, _articleType, _abstract
    :: ReaderElement' (Maybe Text)

_mainHtml :: ReaderElement' (Maybe PaperMainText)
_doi :: ReaderElement' Text
_year :: ReaderElement' (Maybe Int)
_authors :: ReaderElement' [Text]
_publisher :: ReaderElement' (Maybe Text)

_refs :: ReaderElement' [Reference]
_figs :: ReaderElement' [Figure]

 
_annualRevReader = defaultReader {
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
  figs = onlyFullL _figs
}

annualRevReader
  = _annualRevReader {
      readerName = (\_ -> "Annual Reviews")
      ,abstract = absLevel _abstract
      ,mainHtml = onlyFull _mainHtml
    }

_articleType _ _ = Nothing -- headm . getMeta "dc.Type"

_supportedUrl _ url =
  if "http://www.annualreviews.org/doi/full/" `T.isPrefixOf` url then
    Just SFullText
  else
    Nothing

_doi _ = fromMaybe "" . headm . getMeta "dc.Identifier"

_journal _ = inner . queryT [jq| h1.journalTitle |]
_publisher _ = headm .getMeta "dc.Publisher"

_title _ = inner . queryT [jq| h1.arttitle |]


-- ToDo: This is ad hoc. Rewrite these with Regexp.
_volume _ c = let ns = queryT [jq| span.breadcrumbs > a |] c in
                if length ns >= 3 then
                  fmap (fst . T.breakOn "-") $ (inner . (:[]) . (!! 2)) ns >>= dropmt 7
                else
                  Nothing
_pageFrom _ c =
  fmap (fst . T.breakOn "-" . snd . T.breakOn ": ") (inner $ queryT [jq| div.issueInfo |] c)
    >>= dropmt 2
_pageTo _ c = fmap (snd . T.breakOnEnd "-" . fst . T.breakOn " (") (inner $ queryT [jq| div.issueInfo |] c)


_year _ c = fmap (read . T.unpack) ((headm . getMeta "dc.Date") c >>= takemt 4)

_authors _ = getMeta "dc.Creator"

_abstract _ = inner . queryT [jq| div.abstractSection |]
_mainHtml _ = fmap FlatHtml . inner . map fromNode . removeQueries ["div.figureThumbnail", "div.chooseSections","div.headerSelect"] . map node . queryT [jq| div.NLM_sec_level_1 |]

_refs _ cur =
  let
    cs = queryT [jq| #referencesTab > ul > li |] cur
    mkRef c =
      let
        mid = eid (node c)
        mname = do
          n <- inner $ queryT [jq| span.position |] c
          maybeText $ fst $ T.breakOn "." n
        mcittxt = do
          h <- inner $ queryT [jq| div.citation |] c
          t <- headMay $ T.splitOn "<script" h
          return $ T.replace "</i>" "" $ T.replace "<i>" "" t
        murl = do
          txt <- mcittxt
          return $ T.concat ["http://alocator.web.fc2.com/?q=",txt,"&redirect=yes"]
      in
        Reference
          <$> mid
          <*> mname
          <*> Just Nothing  -- Stub
          <*> Just mcittxt
          <*> Just murl
  in
    catMaybes $ map mkRef cs
 
_figs _ cur =
  let
    cs = queryT [jq| #figuresTab |] cur
  in
    []  -- Stub.

