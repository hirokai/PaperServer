-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.NatureA
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
-- For Nature (new format) article
--

{-# LANGUAGE DoAndIfThenElse #-}
module Parser.Publisher.NatureA (
    natureAReader
) where

import Parser.Import
import Text.XML
import Text.XML.Cursor as C
-- import Data.Maybe
import qualified Data.Text as T
import Data.Text.Lazy  (toStrict)
import Data.List
import Parser.Utils
import qualified Data.Map as M

-- import Debug.Trace

import Parser.Publisher.NatureCommon (_title,_volume,_pageFrom,_pageTo)

natureAReader :: PaperReader
natureAReader = defaultReader {
  supportedUrl = _supportedUrl,
  supported = _supported,
  title = anyLevel _title,
  abstract = absLevel _abstract,
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
  toc = onlyFull _toc,
  mainHtml = onlyFull _mainHtml,
  readerName = _readerName
}

_readerName _ = "NatureA"

_supportedUrl _ url = boolToSupp $ "http://www.nature.com/nature/" `T.isPrefixOf` url
_supported r url c = boolToSupp  $ isJust ((supportedUrl r) r url) && ((articleType r) r SUndecidable c == Just "Article")

_mainHtml _ = fmap FlatHtml . render . map subLink . nofignav . nolater .
              noabstract . map node . concatMap (element "section") . descendant

_doi _ = innerText . queryT [jq|dd.doi|]

_journal _ cursor = Just $ head $ map (T.strip . head) $ filter (\x -> not $ null x) $ cursor $// element "p" &| attributeIs "class" "article-type" &.// content

_year _ cursor = headm (getMeta "DC.date" cursor) >>= f
    where
      f s | T.length s >= 4 = Just $ read $ T.unpack (T.take 4 s)
          | otherwise = Nothing                  

_authors _ = getMeta "DC.creator"

_abstract _ cur = if null es then Nothing else (Just . toStrict . renderNodes . (:[]) . node . head) es
    where
     es = cur $| query "#abstract p" >=> child

-- _title _ cursor = Just $ T.concat $ cursor $| query "h1.article-heading" &.// content

-- _articleType _ cursor = Just $ head $ map (T.strip . last) $ filter (\x -> not $ null x) $
--                      cursor $// element "p" &| attributeIs "class" "article-type" &.// content

_articleType _ cursor = headm $ map (T.strip . last) $ filter (\x -> not $ null x) $
                      cursor $// element "p" &| attributeIs "class" "article-type" &.// content

_refs _ cur = map r ns
    where
      ns = map node $ cur $| query "ol.references > li"
      r n@(NodeElement (Element name as cs)) = Reference (refid as) (refname as)(Just emptyCitation) (Just (txt cs)) (url n)
      refid as = fromMaybe "" (M.lookup "id" as)
      refname as = T.drop 3 (refid as)
      txt cs = T.strip $ T.concat $ map innerTextN $ takeWhile g cs
      g (NodeElement (Element name as cs)) = name /= "ul"
      g _ = True
      url n = let cs = fromNode n $// element "a" >=> attribute "href" in
                if null cs then Nothing else Just (last cs)
      f s = s

_figs _ cur = []

_toc _ cur = Just $ (\x -> traceShow x x) $ T.intercalate "::" $ map (innerTextN . node) $ nav $| query "li"
    where
      nav = head $ cur $| query "section nav"


subLink :: Node->Node
subLink (NodeElement (Element n as cs)) = NodeElement (Element n as (map f cs))
  where
    f (NodeElement (Element nn as cs))
      | nameLocalName nn == "a" = NodeElement (Element nn (M.update g "href" as) (map subLink cs)) 
      | otherwise = NodeElement (Element nn as (map subLink cs))
    f n = n
    g url = if length tok < 2 then Just url else Just ("#" `T.append` last tok)
      where
        tok = T.splitOn "#" url
subLink n = n


nofignav = map (remove (\n -> ename n `elem` map Just ["nav","figure"]))

noabstract :: [Node]->[Node]
noabstract ns = tail' $ dropWhile f ns
  where
    f (NodeElement (Element _ _ cs)) = any g cs
    f _ = True
    g (NodeElement (Element _ as _)) = Just "abstract" /= M.lookup "id" as
    g _ = False

nolater :: [Node]->[Node]
nolater ns = fst $ break f ns
  where
    f (NodeElement (Element _ _ cs)) = any g cs
    f _ = True
    g (NodeElement (Element _ as _)) = Just "references" == M.lookup "id" as
    g _ = False

defParse :: Cursor -> Maybe T.Text
defParse _ = Nothing

{-
doi cursor = T.concat $ cursor $// element "dd"
               >=> attributeIs "class" "doi"
               &// content
-}


-- This does not work correctly.
--paperType c = Just $ T.strip $ (trace (show es) es)
--  where es = (innerHtml . (:[]) . last) $ (c $| query "p.article-type" >=> child)


