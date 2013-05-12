-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.PLoSONE
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
-- For PLoS Journals
--

module Parser.Publisher.PLoSONE (
   plosReader 
) where

import Parser.Import
import Text.XML.Cursor as C
import Text.XML (Document)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Control.Applicative
import Control.Lens
import Parser.Utils

_plosReader = defaultReader {
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
  figs = onlyFullL _figs,
  abstract = absLevel _abstract,
  mainHtml = onlyFull _mainHtml,
  parsePaper = plosParsePaper
}

plosReader = _plosReader {readerName=(\_ -> "PLoS")}

_plosReader :: PaperReader
plosReader :: PaperReader
_supportedUrl :: PaperReader -> Text -> Maybe SupportLevel
plosParsePaper :: PaperReader
                        -> Url -> Text -> Text.XML.Document -> IO Paper

hostDomain :: Text -> Text

_title, _journal, _volume, _pageFrom, _pageTo, _articleType, _abstract
    :: ReaderElement' (Maybe Text)

_mainHtml :: ReaderElement' (Maybe PaperMainText)
_doi :: ReaderElement' Text
_year :: ReaderElement' (Maybe Int)
_authors :: ReaderElement' [Text]
_publisher :: ReaderElement' (Maybe Text)

_supportedUrl _ url = boolToSupp $ any (`T.isPrefixOf` url) js
  where js = ["http://www.plosone.org/article/"
              , "http://www.ploscompbiol.org/article/"
              , "http://www.plosbiology.org/article/"
              , "http://www.plosmedicine.org/article/"
              , "http://www.plosgenetics.org/article/"
              , "http://www.plospathogens.org/article/"
              , "http://www.plosntds.org/article/"
              ]

plosParsePaper r url html doc = do
  paper <- defParsePaper r url html doc   -- This uses _acsReader (see the definition of defParsePaper)
  let
    rootCur = fromDocument doc
    resources = getResources url rootCur
  return $ paper & paperResources .~ resources

hostDomain url = (`T.append` ".org") $ fst $ T.breakOn ".org" url
hostDomain' = "http://www.plosone.org"  -- Stub: Is this fixed domain okay?

getResources :: Text -> Cursor -> [Resource]
getResources url_ cur =
  let
    cs = queryT [jq| img.inline-graphic |] cur
    imgs = map (T.replace "&amp;" "&") $ catMaybes $ map (headMay . attribute "src") cs
    mkUrl url path = T.append (hostDomain url) path
    getPlosMime :: Text -> Text
    getPlosMime url
      = fromMaybe "image/png" $
          case snd $ T.breakOnEnd "&representation=" url of
            "PNG" -> Just "image/png"
            "JPEG" -> Just "image/jpeg"
            "GIF" -> Just "image/gif"
            _ -> Nothing
    mkRes idx url = Resource (T.pack $ "img" ++ show idx) url (getPlosMime url)
  in
    zipWith mkRes [1..] (map (mkUrl url_) imgs)


_doi _ = fromMaybe "" . headm . getMeta "citation_doi"

_articleType _ = inner . queryT [jq| #articleType |]

_journal _ = headm . getMeta "citation_journal_title"
_publisher _ = headm .getMeta "citation_publisher"

_title _ = headm . getMeta "citation_title"

-- Somehow this doubles the title.
-- _title _ = inner . queryT [jq| h1[property="dc:title"] |]

_volume _ = headm . getMeta "citation_volume"
_pageFrom _ = headm . getMeta "citation_firstpage"
_pageTo _ = headm . getMeta "citation_lastpage"
_year _ c = fmap (read . T.unpack) ((headm . getMeta "citation_date") c >>= takemt 4)

_authors _ = getMeta "citation_author"

_abstract _ = inner . queryT [jq| div.abstract > p |]
_mainHtml _ c = fmap FlatHtml $ render $ removeQueries ["div.figure"] $ map node $ concatMap (\d -> query d c) divs
  where divs = map (("#section" ++) . show) [1..30]  --Ad hoc

_refs _ c = catMaybes . map parseCit $
              zip (getMeta "citation_reference" c)
                  $ (map innerTextN . removeQueries ["span.label","a.find"] . map node . queryT [jq| ol.references > li|]) c


_figs _ cur =
  let
    cs = queryT [jq| div.figure |] cur
    mkFig c =
      let
        id_ = eid (node c)
        name = do
          e <- headMay $ queryT [jq| p > strong > strong |] c
          inner [e]
        cap = maybeText $ toStrict $ renderNodes $
                  -- removeQueries ["strong > strong"] $
                  map node $ queryT [jq| p |] c
        img = do
          a <- headMay $ queryT [jq| div.figure a |] c
          s <- headMay $ attribute "href" a
          return $ T.append hostDomain' s
        coarse = Just . fromMaybe ""
      in
        Figure <$> id_ <*> name <*> cap <*> img
  in
    catMaybes $ map mkFig cs

-- ToDo: Multiple authors support
parseCit :: (T.Text,T.Text) -> Maybe Reference
parseCit (meta,cittext) = (toRef . M.fromList . catMaybes . map (toTuple . T.splitOn "=") . T.splitOn "; ") meta
  where
    toTuple [a,b] = Just (a,b)
    toTuple _ = Nothing
    toRef :: M.Map T.Text T.Text -> Maybe Reference
    toRef m = traceShow f f
      where
        f = Reference <$> num <*> num <*> Just cit <*> Just (maybeText cittext) <*> Just Nothing
        num = M.lookup "citation_number" m
        cit = Just $ emptyCitation{
                _citationTitle=M.lookup "citation_title" m,
                _citationJournal=M.lookup "citation_journal_title" m,
                _citationVolume=M.lookup "citation_volume" m
              }

