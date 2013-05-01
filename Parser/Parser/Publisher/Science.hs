-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.Science
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
-- For Science
--

module Parser.Publisher.Science (
  scienceReader,
  stkeReader
) where

import Import

import Parser.Utils
import qualified Data.Text as T
import Data.Text.Read

_scienceReader = defaultReader{
  doi = anyLevel _doi,
  journal = anyLevel _journal,
  publisher = anyLevel _publisher,
  volume = anyLevel _volume,
  pageFrom = anyLevel _pageFrom,
  year = anyLevel _year,
  authors = anyLevel _authors,
  articleType = anyLevel _articleType
}

scienceReader = _scienceReader {title=anyLevel _titleS,supportedUrl=_supportedUrlS,pageTo=anyLevel _pageToS}
stkeReader = _scienceReader {title=anyLevel _titleSTKE,supportedUrl=_supportedUrlSTKE}

_doi _ = fromMaybe "" . headm . getMeta "citation_doi"

_supportedUrlSTKE _ url
  = if any (`T.isPrefixOf` url) ["http://stke.sciencemag.org/cgi/content/full/"] then
      Just SCitation
    else
      Nothing
      
_supportedUrlS _ url = 
    if "http://www.sciencemag.org/content/" `T.isPrefixOf` url &&
                          ".full" `T.isSuffixOf` url
    then
      Just SCitation
    else
      Nothing

-- _title :: PaperReader -> Cursor -> Maybe Text
_titleSTKE _ = inner . queryT [jq| h2 |]
_titleS _ = inner . queryT [jq| #article-title-1 |]


_journal _ = headm . getMeta "citation_journal_title"
_publisher _ = headm . getMeta "citation_publisher"

_pageFrom _ = headm . getMeta "citation_firstpage"
_authors _ = getMeta "citation_authors"
_volume _ = headm . getMeta "citation_volume"
_year _ cur = (fmap (T.reverse . T.take 4 . T.reverse) . headm . getMeta "citation_date") cur >>= r
  where r a = case decimal a of
              Left err -> Nothing
              Right (v,t) -> Just v
_articleType _ cur = (fmap ( (:[])) . headm . queryT [jq| h5 |]) cur >>= inner

_pageToS _ = headm . getMeta "citation_lastpage"


