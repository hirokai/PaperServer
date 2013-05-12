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

import Parser.Import
-- import Safe
import Parser.Utils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Read
import Control.Applicative
import Text.XML.Cursor

_scienceReader = defaultReader{
  doi = anyLevel _doi,
  journal = anyLevel _journal,
  publisher = anyLevel _publisher,
  volume = anyLevel _volume,
  pageFrom = anyLevel _pageFrom,
  year = anyLevel _year,
  authors = anyLevel _authors,
  articleType = anyLevel _articleType,
  figs = onlyFullL _figsS
}

_scienceReader :: PaperReader
scienceReader :: PaperReader
stkeReader :: PaperReader
_supportedUrlSTKE :: PaperReader -> Text -> Maybe SupportLevel
_supportedUrlS :: PaperReader -> Text -> Maybe SupportLevel
_supportedS :: PaperReader -> Text -> Cursor -> Maybe SupportLevel


_titleSTKE, _titleS, _journal, _volume, _articleType
    :: ReaderElement' (Maybe Text)

_doi :: ReaderElement' Text
_year :: ReaderElement' (Maybe Int)
_authors :: ReaderElement' [Text]
_publisher :: ReaderElement' (Maybe Text)

_pageFrom, _pageToS :: ReaderElement' (Maybe Text)
_abstractS :: ReaderElement (Maybe Text)
_mainHtmlS :: ReaderElement (Maybe PaperMainText)


scienceReader = _scienceReader {
                  title=anyLevel _titleS
                  , supported=_supportedS
                  , supportedUrl=_supportedUrlS
                  , pageTo=anyLevel _pageToS
                  , abstract=_abstractS
                  , mainHtml=_mainHtmlS
                  }
stkeReader = _scienceReader {
                title=anyLevel _titleSTKE
                , supportedUrl=_supportedUrlSTKE
                }

_doi _ = fromMaybe "" . headm . getMeta "citation_doi"

_supportedUrlSTKE _ url
  = if any (`T.isPrefixOf` url) ["http://stke.sciencemag.org/cgi/content/full/"] then
      Just SCitation
    else
      Nothing
      
_supportedUrlS _ url = 
  if "http://www.sciencemag.org/content/" `T.isPrefixOf` url then
    if ".full" `T.isSuffixOf` url then
      Just SFullText
    else if ".abstract" `T.isSuffixOf` url then
      Just SAbstract
    else
      Nothing 
  else
    Nothing

_supportedS r url cur =
  case _supportedUrlS r url of
    Just SFullText ->
      if (not . null $ queryT [jq| div.fulltext-view > p |] cur) then
        Just SFullText
      else
        Just SAbstract
    Just SAbstract -> Just SAbstract
    s -> s




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

_abstractS _ support
  | support == SFullText || support == SAbstract = inner . queryT [jq| div.section.abstract > p |]
  | otherwise = const Nothing

_mainHtmlS _ SFullText c = fmap FlatHtml $ maybeText $ TL.toStrict $ toHtml $ queryT [jq| div.fulltext-view > p |] c
_mainHtmlS _ _ c = Nothing


_figsS _ c =
  let
    cs = queryT [jq| div.fig |] c
    mkFig d =
      let
        id_ = eid $ node d
        imgdiv = headm $ queryT [jq| div.fig-inline img |] d
        name = imgdiv >>= (headMay . attribute "alt")
        cap = imgdiv >>= (inner . queryT [jq| div.fig-caption |])
        img = imgdiv >>= (headMay . attribute "src")
      in
        Figure <$> id_ <*> name <*> cap <*> img
  in
    catMaybes $ map mkFig cs
