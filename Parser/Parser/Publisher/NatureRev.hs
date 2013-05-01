-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.NatureRev
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
-- For Nature Review Journals
--
{-# LANGUAGE OverloadedStrings #-}
module Parser.Publisher.NatureRev (
    natureRevReader
) where

import Import
import Text.XML.Cursor as C
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>),(<|>))
import Parser.Utils

import Parser.Publisher.NatureCommon hiding (_year,_articleType,_title)

-- Checked on
-- http://www.nature.com/nri/journal/v11/n10/full/nri3066.html

natureRevReader = defaultReader {
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
  mainHtml = onlyFull _mainHtml,
  readerName = _readerName
}

hostDomain = "http://nature.com"

_supportedUrl _ url = boolToSupp $
  any (`T.isPrefixOf` url)
      (map ("http://www.nature.com/" `T.append`)
            ["nri/","nrm/","nrc/","nrd/","nrg/","nrmicro/","nrn",
            "nrcardio/","nrclinonc/","nrendo/","nrgastro/","nrneph/","nrneurol/","nrrheum/","nrurol/"] ::[T.Text])
  && "/full/" `T.isInfixOf` url
 
_articleType _ = headm . getMeta "prism.section"

_title _ = inner . queryT [jq| h2#atl |]

_year _ = fmap (read . T.unpack . T.take 4) . headm . getMeta "prism.publicationDate"

_readerName _ = "Nature reviews"

_abstract _ = inner . queryT [jq| p.lead |]
_mainHtml _ = fmap FlatHtml . maybeText . toStrict . renderNodes . removeQuery ".figure-table" . map node . queryT [jq| #articlebody |]

_refs _ = map extract . zip [1..] . queryT [jq| ol.references > li|]

extract (idx,cur)
  = Reference
            (fromMaybe "" $ eid (node cur))
            (T.pack $ show idx)
            cit
            --(maybeText $ toStrict $ renderNodes $ removeQuery "b" $ map node $ concatMap child $ queryT [jq| p.details |] cur)
            Nothing -- stub
            url
                    
  where
    getUrl cur = (headm . queryT [jq| ul.article-views > li > a |]) cur >>= f
    f = headm . attribute "href"
    cit = Just $ emptyCitation{_citationDoi=doi,_citationUrl=url,_citationJournal=journal,_citationVolume=volume}
    doi = Nothing -- stub
    url = getUrl cur
    journal = inner $ queryT [jq| span.atl |] cur
    volume = inner $ queryT [jq| span.vid |] cur
    year = inner $ queryT [jq| span.cite-month-year |] cur

_figs :: PaperReader -> Cursor -> [Figure]
_figs _ cur =
  let
    cs = queryT [jq| div.figure-table |] cur
    mkFig c =
      let
        id_ = eid $ node c
        name = (maybeText $ (T.intercalate " " . take 2 . T.splitOn " " . innerText . element "h5") c) <|> id_
        cap = render . removeQueries ["img.thumb","span.cleardiv","ul.options"] . (:[]) . node $ c 
        img = do
          e <- headMay $ queryT [jq| img.thumb |] c
          src <- headMay $ attribute "src" e
          return $ T.append hostDomain $ T.replace "/thumbs/" "/images/" src
      in
        Figure <$> id_ <*> name <*> cap <*> img
  in
    catMaybes $ map mkFig cs
