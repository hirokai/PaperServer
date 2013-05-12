-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.Nature2
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
-- For Nature Immunology and many other nature subjournals
-- It seems all Nature journals except Nature and review journals belong to this category.
--

 {-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Parser.Publisher.Nature2 (
    nature2AReader,nature2LReader,nature2OtherReader
) where

import Parser.Import
import Text.XML
import Text.XML.Cursor as C
-- import Data.Maybe
import Data.Text.Lazy  (toStrict)
-- import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative((<|>),(<$>),(<*>))
-- import Safe

import Data.Tree
import Control.Lens hiding (element)

import Settings
import Parser.Utils

import Text.Regex.PCRE.Rex

import Parser.Publisher.NatureCommon hiding (_title)

_nature2Reader = defaultReader {
  supportedUrl = _supportedUrl,
--  supported = _supported,
  title = anyLevel _title,
--  abstract = _abstract,
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
--  mainHtml = _mainHtml,
  publisher = _publisher
  , parsePaper = nature2ParsePaper
}

nature2AReader
  = _nature2Reader{
      supported=_supportedA
      ,mainHtml=onlyFull _mainHtmlA
      ,toc=onlyFull tocA
      ,abstract=absLevel abstractA
      ,readerName=_readerNameA
    } 

nature2LReader
  = _nature2Reader{
      supported=_supportedL
      ,mainHtml=onlyFull _mainHtmlL
      ,toc=onlyFull tocL
      ,abstract=absLevel abstractL
      ,readerName=_readerNameL
    } 

nature2OtherReader
  = _nature2Reader{
      supported=_supportedOther
      ,mainHtml=onlyFull _mainHtmlOther
      ,toc=onlyFull tocL
      ,abstract= absLevel abstractOther
      ,readerName=_readerNameOther
    } 


_readerNameA _ = "Nature2A"
_readerNameL _ = "Nature2L"
_readerNameOther _ = "Nature2 Other"


prefixList :: [Text]
prefixList = map ("http://www.nature.com/" `T.append`)
                   ["nmat/","nphys/","ni/","nchembio/","ncb/","nchem/","nbt/","nnano/","nsmb/","nmeth/","nclimate/",
                   "ng/","nm","neuro","nphoton","nprot","ncomms"]

_supportedUrl _ url = 
  if any (`T.isPrefixOf` url) prefixList then
    if "/full/" `T.isInfixOf` url then
      Just SFullText
    else if "/abs/" `T.isInfixOf` url then
      Just SAbstract
    else
      Nothing
  else
    Nothing

_supportedA r url c = boolToSupp $ (isJust $ _supportedUrl r url) && isJust (_title r c)
                        && _articleType r c `elem` map Just ["Article","Review","Protocol"]


_supportedL r url c = boolToSupp $ (isJust $ _supportedUrl r url) && _articleType r c `elem` map Just ["Letter","Brief Communication"]
_supportedOther r url c
  = boolToSupp $ (isJust $ _supportedUrl r url) &&
      _articleType r c `elem` (map Just ["Research Highlights","Editorial","News and Views","Retraction"])
          -- Stub: add more.
-- ToDo: Seems "News and Views" needs special treatment? Test 10.1038/nsmb.2552


nature2ParsePaper r url html doc = do
  paper <- defParsePaper r url html doc   -- This uses _acsReader (see the definition of defParsePaper)
  let
    rootCur = fromDocument doc
    sec = getSections rootCur
    -- resources = getResources rootCur
  return $ paper & paperSections .~ sec -- & paperResources .~ resources


-- Copied from ACS.hs and modified.
-- Stub: This only gives the first depths.
getSections :: Cursor -> Maybe SectionInfo
getSections doc =
  let
    sec1 = queryT [jq| section h1.section-heading |] doc
    sec1_titles_str = map (innerText . (:[])) sec1
  in
    case sec1_titles_str of
      [] -> Nothing
      str -> Just $ SectionInfo (Node (rootSection doc) $ map mkSection (zip str sec1)) -- stub: this is only first depth.

rootSection doc = Section "" ""  -- Stub: using a specific cursor may be strange
mkSection :: (T.Text,Cursor) -> Tree Section
mkSection (title,cur) = Node (Section title (fromMaybe "" $ eid $ node cur)) []




_mainHtmlL,_mainHtmlA :: PaperReader -> Cursor -> Maybe PaperMainText
_mainHtmlL _ cursor = fmap FlatHtml . maybeText . T.strip . toStrict .
                        renderNodes . map replaceLinks . nofignav . extractMainL . map node $ (cursor $// element "section")

_mainHtmlA _ cursor = fmap FlatHtml $ (render . map replaceLinks . nofignav . nolater . noabstract . map node) $ (cursor $// element "section")

_mainHtmlOther r cur = _mainHtmlL r cur <|> (fmap FlatHtml $ inner $ queryT [jq| #articlebody |] cur)

{-
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
-}

-- Stub: predicate does not work.
replaceLinks :: Node -> Node
replaceLinks = mapAttr (const True) replaceRefLink . mapAttr (const True)  replaceHref . mapAttr (const True)  replaceImgSrc
  where
    f (NodeElement (Element name attr cs)) = show name == "a"
    f _ = False
    g (NodeElement (Element name attr cs)) = show name == "img"
    g _ = False

mkFileNameTxt = T.pack . mkFileName . T.unpack

hostDomain :: Text
hostDomain = "http://nature.com"

-- different from ACS one. This version is better.
-- ToDo: predicates defined above does not work. Figure out how to get a node name..
mapAttr :: (Node -> Bool) -> (M.Map Name Text -> M.Map Name Text) -> Node -> Node
mapAttr pred func n@(NodeElement (Element name attr cs))
   = NodeElement (Element name (if pred n then func attr else attr) (map (mapAttr pred func) cs))
 -- | otherwise = n
mapAttr _ _ n = n

replaceRefLink attr = 
  let
    f name value
      | name == "href" =
          let
            token = T.splitOn "#" value
          in
            if length token == 2 then
              "#" `T.append` (token !! 1)
            else
              value
      | otherwise = value
    mjs = do
      idtxt <- M.lookup "id" attr
      let idnum = T.drop 9 idtxt
      return $ T.concat ["showRef('",idnum,"');"]
    action = case mjs of
                Just js -> M.insert "onclick" js
                Nothing -> id
  in
     action $ M.mapWithKey f attr

replaceImgSrc :: M.Map Name Text -> M.Map Name Text
replaceImgSrc attr =
  let
    f name value
      | name == "src" =
          if T.length value >= 1 && T.head value == '/' then (T.pack resourceRootUrl) `T.append` (mkFileNameTxt (fullurl value))
          else value
      | otherwise = value
    fullurl path = hostDomain `T.append` path
  in
    M.mapWithKey f attr

replaceHref :: M.Map Name Text -> M.Map Name Text
replaceHref attr =
  let
    f name value
      | name == "href" =
          if T.length value >= 1 && T.head value == '/' then hostDomain `T.append` value
          else value
      | otherwise = value
  in
    M.mapWithKey f attr

-- ToDo: decide if "author-information" is necessary or not.
extractMainL :: [Node]->[Node]
extractMainL ns = filter ((nodeHaving f)) ns
  where
    f n = eid n `elem` map Just ["main","methods","acknowledgments","supplementary-information"]

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


tocA,tocL :: PaperReader -> Cursor -> Maybe T.Text
tocL _ _ = Nothing
tocA _ = Just . T.intercalate "::" . map (innerTextN . node) . nav
  where
    nav = queryT [jq| section nav li |]

_title :: PaperReader -> Cursor -> Maybe T.Text
_title _ c = (inner . queryT [jq| h1.article-heading |]) c <|> (inner . queryT [jq| h2#atl |]) c

abstractA _ = inner . take 1 . queryT [jq| div#abstract p |]

abstractL _ cur = inner $ take 1 $ (queryT [jq| #first-paragraph |] cur) ++
                                        (queryT [jq| #abstract p |] cur)


abstractOther _ = inner . queryT [jq| div.section.first |]

{-
_doi _ cur = fromMaybe "" $ (headm $ getMeta "citation_doi" cur) >>=
                  (\x -> if T.length x > 4 then Just (T.drop 4 x) else Nothing)

-}

_refs _ cur =
  case map node $ queryT [jq| #References > li |] cur of
    [] -> refs2 cur
    ns -> map r ns
  where
    r n@(NodeElement (Element name as cs))
      = Reference
            (refid as)
            (refname as)
            (Just emptyCitation{_citationDoi=getDoi (url n)})
            (Just (txt cs))
            (url n)
    refid as = fromMaybe "" (M.lookup "id" as)
    refname as = T.drop 3 (refid as)
    txt cs = T.strip $ T.concat $ map innerTextN $ takeWhile g cs
    g (NodeElement (Element name as cs)) = name /= "ul"
    g _ = True
    url n = let cs = fromNode n $// element "a" >=> attribute "href" in
              if null cs then Nothing else Just (addDomain $ last cs)
    getDoi (Just u) = fmap T.pack $ [rex| (?{ }10.[0-9]{4}.+)|] (T.unpack u)
    getDoi Nothing = Nothing
    f s = s
    addDomain s = if T.length s > 0 && T.head s == '/' then
                      T.append "http://www.nature.com" s
                  else
                      s

refs2 cur =
  let
    cs = queryT [jq| .references > li |] cur
    mkRef c =
      let
        id = fromMaybe "" $ eid (node c)
        name = T.drop 3 id
        cit = Just $ mkCit2 link c
        cittxt = do
          h <- (headMay . removeQuery "ul.has-ref-links") [node c]
          (maybeText . T.strip . innerTextN ) h
        uls = queryT [jq| ul.has-ref-links |] c
        link = do
          ul <- uls `atMay` (length uls-2)
          a <- headMay $ element "a" ul
          headMay $ attribute "href" a
      in
        Reference id name cit cittxt link
    mkCit2 url c =
      let
        takehead  = fmap (innerTextN . node) . headMay
        doi       = Nothing
        title     = takehead $ queryT [jq| span.title |] c
        journal   = takehead $ queryT [jq| span.source-title |] c
        year      = (takehead $ queryT [jq| span.year |] c) >>= (readMay . T.unpack)
        volume    = takehead $ queryT [jq| span.volume |] c
        pagefrom  = takehead $ queryT [jq| span.start-page |] c
        pageto    = takehead $ queryT [jq| span.end-page |] c
        authors   = map (innerTextN . node) $ queryT [jq| span.author |] c
        publisher = Nothing
        type_     = Nothing
      in
        Citation doi url title journal year volume pagefrom pageto authors publisher type_
  in
    map mkRef cs

_figs _ = catMaybes . map mkFig . queryT [jq| div.figure |]

mkFig :: Cursor -> Maybe Figure
mkFig cur =
  let
    imgelem = headMay $ queryT [jq| img |] cur
    id_ = eid . node $ cur
    name :: Maybe Text
    name = do
      txt <- inner $ queryT [jq| span.legend |] cur
      (headMay . T.splitOn ":") (txt :: Text)
    src = do
      elem <- imgelem
      s <- headMay $ attribute "src" elem
      return $ (T.replace "/images_article/" "/images/") s
    caption :: Maybe Text
    caption = Just $ (fromMaybe "" $ inner $ queryT [jq| figcaption |] cur) `T.append`
               (fromMaybe "" $ inner $ queryT [jq| div.description |] cur)
    img = fmap ("http://www.nature.com" `T.append`) src
  in
    Figure <$> id_ <*> name <*> caption <*> img

