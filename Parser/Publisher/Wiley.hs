{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.Wiley
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
-- For Wiley journals
--

module Parser.Publisher.Wiley (
  wileyReaderC    
) where

import Parser.Import
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Read

import Control.Applicative ((<$>),(<*>))
import Control.Monad

import Text.XML.Cursor as C

import Parser.Utils

_wileyReader :: PaperReader
wileyReaderC :: PaperReader
_articleType :: PaperReader -> Cursor -> Maybe Text
_supportedUrl :: PaperReader -> Text -> Maybe SupportLevel
_doi :: PaperReader -> Cursor -> Text
_journal :: PaperReader -> Cursor -> Maybe Text
_publisher :: PaperReader -> Cursor -> Maybe Text
_title :: PaperReader -> Cursor -> Maybe Text
_volume :: PaperReader -> Cursor -> Maybe Text
_pageFrom :: PaperReader -> Cursor -> Maybe Text
_pageTo :: PaperReader -> Cursor -> Maybe Text
_year :: PaperReader -> Cursor -> Maybe Int
_authors :: PaperReader -> Cursor -> [Text]
_abstractC :: PaperReader -> Cursor -> Maybe Text
_mainHtmlC :: PaperReader -> Cursor -> Maybe PaperMainText
_refs :: PaperReader -> Cursor -> [Reference]
mkRef :: Cursor -> Reference

_wileyReader = defaultReader {
  supportedUrl = _supportedUrl,
  doi = anyLevel _doi,
  journal = anyLevel _journal,
  publisher = anyLevel _publisher,
  title = anyLevel _title,
  volume = anyLevel _volume,
  pageFrom = anyLevel _pageFrom,
  pageTo = anyLevel _pageTo,
  year = anyLevel _year,
  figs = onlyFullL _figs,  
  authors = anyLevel _authors,
  articleType = anyLevel _articleType,
  refs = onlyFullL _refs
}

wileyReaderC
  = _wileyReader {
      readerName = (\_ -> "Wiley communication")
      ,abstract = absLevel _abstractC
      ,mainHtml = onlyFull _mainHtmlC
    }

_articleType _ = join . fmap (inner . (:[])) . headm . queryT [jq| p.articleCategory |]

_supportedUrl _ url =
  if "http://onlinelibrary.wiley.com/doi/" `T.isPrefixOf` url then
    if "/full" `T.isInfixOf` url then
      Just SFullText
    else if "/abstract" `T.isInfixOf` url then
      Just SAbstract
    else
      Nothing
  else
    Nothing 

_doi _ = fromMaybe "" . headm . getMeta "citation_doi"

_journal _ = headm . getMeta "citation_journal_title"
_publisher _ = headm .getMeta "citation_publisher"

_title _ = inner . map fromNode . removeTags ["a"] . map node . queryT [jq| span.mainTitle |]

_volume _ = headm . getMeta "citation_volume"
_pageFrom _ = headm . getMeta "citation_firstpage"
_pageTo _ = headm . getMeta "citation_lastpage"
_year _ c = fmap (read . T.unpack) ((headm . getMeta "citation_publication_date") c >>= takemt 4)

_authors _ = getMeta "citation_author"

_abstractC _ = inner . queryT [jq| div#graphicalAbstract |] -- Note: Abstract is not shown in all full text pages.
_mainHtmlC _ = fmap FlatHtml . inner . map fromNode . removeQueries ["select.jumpSelect", "div.keywords","div.figure","#supp-info","div.bibliography","div#abstract","ol.jumpList"] . map node . queryT [jq| #fulltext |]

_refs _ = map mkRef . queryT [jq| #wol-references > ul > li |]

mkRef cur = Reference
            cid
            num
            cit
            txt
            Nothing
          
  where
    -- Ugly, should be refactored.
    citcur = headm $ queryT [jq| cite |] cur
    cid = fromMaybe "" $ do
      cc <- citcur
      (eid . node) cc
    txt = do
      cc <- citcur
      maybeText $ toStrict $ innerText [cc]
    num = toStrict . innerText $ queryT [jq| span.bullet |] cur
    cit = Just $ emptyCitation{_citationAuthors=authors}
    authors = fromMaybe [] $ fmap (map (toStrict . innerText) . queryT [jq| span.author |]) citcur
    year = do
        cc <- citcur
        h <- headm $ queryT [jq| span.pubYear |] cc
        val <- (fmap snd . eitherMaybe . decimal . toStrict . innerText) h
        return val


_figs _ cur =
  let
    cs = queryT [jq| div.figure |] cur
    mkFig :: Cursor -> Maybe Figure
    mkFig c =
      let
        dropName = T.drop (T.length "</span>") . snd . T.breakOn "</span>"
        id_ = eid (node c)
        name = maybeText $ toStrict $ innerHtml $ queryT [jq| div > p > span.label |] c
        caption = do
          p <- headMay $ queryT [jq| div.caption > p |] c
          ts <- (tailMay . child) p
          txt <- (render . map node) ts 
          return $ dropName txt
        img = do
          a <- headMay $ queryT [jq| a.figZoom |] c
          headMay $ attribute "href" a
      in
        Figure <$> id_ <*> name <*> caption <*> img
  in
    catMaybes (map mkFig cs)
