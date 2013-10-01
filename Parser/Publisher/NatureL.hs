{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.NatureL
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
-- For Nature (new format) letter
--


{-# LANGUAGE DoAndIfThenElse, OverloadedStrings #-}

module Parser.Publisher.NatureL (
    natureLReader
) where

import Parser.Import
import Text.XML
import Text.XML.Cursor as C
import Data.List
import Control.Applicative
import qualified Data.Map as M
import Parser.Utils

import Parser.Publisher.NatureCommon (_publisher,_pageFrom,_pageTo,_volume,_authors,_year)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

natureLReader :: PaperReader
natureLReader = defaultReader {
  supportedUrl = _supportedUrl,
  supported = _supported,
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
  toc = onlyFull _toc,
  mainHtml = onlyFull _mainHtml,
  publisher = _publisher,
  readerName = _readerName
}


--ToDo: refactor scraping by using dom-selector

_readerName _ = "NatureL"

_supportedUrl _ url = boolToSupp $ "http://www.nature.com/nature/" `T.isPrefixOf` url
_supported r url c = boolToSupp $ isJust ((supportedUrl r) r url) && ((articleType r) r SUndecidable c == Just "Letter")
_title _ = inner . queryT [jq| h1.article-heading |]

_abstract _ cur = render $ take 1 $ map node $ (queryT [jq| #first-paragraph |] cur) ++
                                        (queryT [jq| #abstract p |] cur)


-- These are different from ones in NatureCommon
_doi _ = TL.toStrict . innerText . queryT [jq| dd.doi |]

_journal _ cursor = Just $ head $ map (T.strip . head) $ filter (\x -> not $ null x) $ cursor $// element "p" &| attributeIs "class" "article-type" &.// content

_articleType _ cursor =
  (headMay $ getMeta "prism.section" cursor)
  <|> (headm $ map (T.strip . last) $ filter (\x -> not $ null x) $
                      cursor $// element "p" &| attributeIs "class" "article-type" &.// content)
-- --------
--
--


_refs _ cur = map r ns
    where
      ns = map node $ cur $| queryT [jq| ol.references > li |]
      r n@(NodeElement (Element name as cs)) = Reference (refid as) (refname as)(Just def) (Just (txt cs)) (url n)
      refid as = fromMaybe "" (M.lookup "id" as)
      refname as = T.drop 3 (refid as)
      txt cs = T.strip $ TL.toStrict $ innerText $ takeWhile g cs
      g (NodeElement (Element name as cs)) = name /= "ul"
      g _ = True
      url n = let cs = fromNode n $// element "a" >=> attribute "href" in
                if null cs then Nothing else Just (last cs)
      f s = s

_figs _ cur = []
_toc _ _ = Nothing
_mainHtml _ cursor = fmap FlatHtml $ render $ map subLink $ nofignav $ extractMainL $ map node $ (cursor $// element "section")

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

extractMainL :: [Node]->[Node]
extractMainL ns = filter ((nodeHaving f)) ns
  where
    f n = eid n `elem` map Just ["main","methods"]

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


