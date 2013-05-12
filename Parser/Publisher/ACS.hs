--
-- Parser.ACS
-- 4 journals: "ja","am","cb","jo"
--

module Parser.Publisher.ACS (
    acsAReader, acsLReader
  ) where

import Prelude
import Parser.Import

import Text.XML
import Text.XML.Cursor as C
import Data.Text.Lazy as TL (toStrict, concat)
import Data.List

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec

import Parser.Utils
import Control.Lens
import Data.Tree
-- import Safe
-- import System.IO
-- import Control.Applicative hiding ((<|>))
-- import Control.Monad
import qualified Data.Map as M
import Settings
-- import Parser.Utils

_acsReader = defaultReader {
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
  toc = onlyFull _toc,
  publisher = anyLevel _publisher,
  parsePaper = acsParsePaper

}


-- ToDo: Some papers don't work.
--
-- Reader Not found
-- http://pubs.acs.org/doi/full/10.1021/cm400196w

-- Figure odd.
-- http://pubs.acs.org/doi/full/10.1021/ie302928v
--Too large


acsAReader = _acsReader {supported=_supportedA,mainHtml=onlyFull _mainHtmlA,readerName = \_ -> "ACS Article"}

acsLReader = _acsReader {supported=_supportedL,mainHtml=onlyFull _mainHtmlL,readerName = \_ -> "ACS Letter"}

fullsupport =
  [   "ed"  -- J. Chem. Ed.
    , "ja"  -- JACS
    , "am"  -- ACS Appl. Mater. Interfaces
    , "cb"  -- ACS Chem. Biol.
    , "cn"  -- ACS Chem. Neurosci.
    , "co"  -- ACS Comb. Sci.
    , "jo"  -- JOC
    , "nl"  -- Nano Lett.
    , "nn"  -- ACS Nano
    , "ol"  -- Org. Lett.
    , "mz"  -- ACS Macro Lett.  
    , "ml"  -- ACS Med. Chem. Lett.
    , "sc"  -- ACS Sustainable Chem. Eng.
    , "sb"  -- ACS Synth. Biol.
    , "ac"  -- Anal. Chem.
    , "jm"  -- J. Med. Chem.
    , "la"  -- Langmuir
    , "ma"  -- Macromolecules
    , "om"  -- ???
    , "cm"  -- Chem. Mater.
    , "cr"  -- Chem. Rev.
    , "bm"  -- Biomacromolecules
    , "bc"  -- Bioconjugate Chem.
    , "tx"  -- Chem. Res. Toxicol.
    , "ar"  -- Acc. Chem. Res.
    , "am"  -- ACS Appl. Mater. Interface
    , "cs"  -- ACS Catal.
    , "bi"  -- Biochemistry
    , "es"  -- Environ. Sci. Technol.
    , "ef"  -- Energy Fuels
    , "cg"  -- Cryst. Growth Des.
    , "ie"  -- Ind. Eng. Chem. Res.
    , "ic"  -- Inorg. Chem.
    , "jf"  -- J. Agric. Food Chem.
    , "je"  -- J. Chem. Eng. Data
    , "ci"  -- J. Chem. Inf. Model.
    , "ct"  -- J. Chem. Theory Comput.
    , "jp"  -- J. Phys. Chem.
    , "mp"  -- Mol. Pharmaceutics
    , "op"  -- Org. Process Res. Dev.
    , "np"  -- J. Nat. Prod.
    , "jz"  -- J. Phys. Chem. Lett.
    , "jp"  -- J. Proteome Res.
    ]


_supportedUrl _ url
  = if "http://pubs.acs.org/doi/" `T.isPrefixOf` url then
      if any (`T.isInfixOf` url) (map ("/full/10.1021/" `T.append`) fullsupport) then
        Just SFullText
      else if any (`T.isInfixOf` url) (map ("/abs/10.1021/" `T.append`) fullsupport) then
        Just SAbstract
      else
        Nothing
    else
      Nothing

_supportedA = supportedWith ["Article", "Research Article", "Articles",
                              "Review",
                              "Featured Article", "Perspective","JOCSynopsis","Laboratory Experiment",
                              "Invited Feature Article"]


_supportedL = supportedWith ["Communication","Letter",
                              "Rapid Report", "Viewpoint","Note","Editorial"]


-- ToDo: simplify this structure.
supportedWith :: [Text] -> PaperReader -> Url -> Cursor -> Maybe SupportLevel
supportedWith ns r url cur
  = case (supportedUrl r) r url of
      Just SFullText ->
        if (articleType r) r SFullText cur `elem` (map Just ns) then
          if (not . null) $ queryT [jq| #articleBody |] cur then
            Just SFullText
          else
            Just SAbstract
        else
          Nothing
      Just SAbstract ->
        Just SAbstract
      _ -> Nothing


acsParsePaper r url html doc = do
  paper <- defParsePaper r url html doc   -- This uses _acsReader (see the definition of defParsePaper)
  let
    rootCur = fromDocument doc
    sec = getSections rootCur
    resources = getResources rootCur
  return $ paper & paperSections .~ sec & paperResources .~ resources

getResources :: Cursor -> [Resource]
getResources doc =
  let
    imgs = queryT [jq| #articleBody img |] doc
    addDomain path
      | T.head path == '/' = "http://pubs.acs.org" `mappend` path
      | otherwise = path
    mkName idx url = (T.pack ("img" ++ show idx),url)
    mkResource (id,url) = Resource id url (mimeFromUrl url)
  in
    map mkResource . zipWith mkName [1..] . map addDomain . nub . catMaybes . map (headMay . attribute "src") $ imgs
  

-- Stub: This only gives the first depths.
getSections :: Cursor -> Maybe SectionInfo
getSections doc =
  let
    sec1 = queryT [jq| div.NLM_sec_level_1 |] doc
    sec1_titles = catMaybes $ map (headMay . queryT [jq| > h2 |]) sec1
    sec1_titles_str = map (innerText . (:[])) sec1_titles
  in
    case sec1_titles_str of
      [] -> Nothing
      str -> Just $ SectionInfo (Node (rootSection doc) $ map mkSection (zip str sec1_titles)) -- stub: this is only first depth.

rootSection doc = Section "" ""  -- Stub: using a specific cursor may be strange
mkSection :: (T.Text,Cursor) -> Tree Section
mkSection (title,cur) = Node (Section title (fromMaybe "" $ eid $ node cur)) []

_pageFrom r c = fst ps
  where ps = pages c
_pageTo r c = snd ps
  where ps = pages c

_abstract _ c = do
  txt <- inner $ queryT [jq|#abstractBox p|] c
  let
    url_raw_m = do
      e <- headMay $ (queryT [jq| div.figure img |] c) <||> (queryT [jq| div#absImg img |] c)
      headMay $ attribute "src" e
    urlm = do
      raw <- url_raw_m
      return $ (T.pack resourceRootUrl) `T.append` (mkFileNameTxt ("http://pubs.acs.org" `mappend` raw))
  return $ T.append
               (maybe "" (\t -> T.concat ["<div alt='Graphical abstract' class='absfig'><img src='",t,"'/></div>"]) urlm)
               txt
_toc _ c = Nothing

_title _ cur = inner $ cur $| query "h1.articleTitle"

-- ToDo: This is really ugly.
_refs _ c = map mkRef $ Data.List.concat li
    where
      li :: [[Cursor]]
      li = map (concatMap (byClass "NLM_citation") . descendant) (c $| query "#references > li")
      mkRef :: Cursor -> Reference
      mkRef c = Reference
                  (getId c)
                  (name c)
                  (Just emptyCitation{_citationDoi=getDoi c})  --Stub
                  (cit c)
                  (getUrl c)
      name :: Cursor -> T.Text
      name c =  case getId c of
                  "" -> ""
                  a -> T.drop 3 a -- Ad hoc
      getId c = fromMaybe "" (headm $ c $| attribute "id")
      cit c = Just $ toStrict $ TL.concat [journal, ", ", vol, ", ", pagef, "-", paget, "(", year, ")"]
        where
          innerq s = innerHtml $ c $| query s
          journal = innerq "span.citation_source-journal"
          vol     = innerq "span.NLM_volume" 
          pagef   = innerq "span.NLM_fpage" 
          paget   = innerq "span.NLM_lpage" 
          year    = innerq "span.NLM_year"
      getUrl c = case getDoi c of
                    Just u -> Just $ predoi `T.append` u
                    Nothing -> Just $ (pre `T.append`) $ fromJust $ cit c
      pre = "http://alocator.web.fc2.com/?redirect=yes&q="
      predoi = "http://dx.doi.org/"
      a :: Cursor -> Maybe Cursor
      a = headm . queryT [jq| div.citationLinks a |]
      getDoi :: Cursor -> Maybe T.Text
      getDoi c | isJust ac = (headm $ attribute "href" (fromJust ac)) >>= extHref
                | otherwise = Nothing
        where ac = a c
      extHref :: T.Text -> Maybe T.Text
      extHref s = maybeText $ decodeUrl $ last $ T.splitOn "&key=" s

_figs :: PaperReader -> Cursor -> [Figure]
_figs _ c = map mkFig $ zip4 id name cap img
    where
      id = map (fromMaybe "") $ map (headMay . attribute "id") fig
      name = tail $ map (T.intercalate " " . take 2 . T.splitOn " " . T.concat . ( $// content)) fig
      img :: [T.Text]
      img = map (T.replace "/small/" "/medium/" . ("http://pubs.acs.org" `T.append`)) $
                    concatMap (head . ($// C.element "img" &| attribute "src")) $ tail fig
      cap :: [T.Text]
--      cap = (map (toStrict . innerHtml) . map (:[])
--             . concatMap (anyElement <=< child <=< query "div.caption")) fig
      cap = tail $ map (fromMaybe "" . render . removeQueries ["a[title='Open Figure Viewer']","a.thumbnail"] . (:[]) . node) fig
      fig :: [Cursor]
      fig = c $| query "div.figure"
      mkFig (id,name,cap,img) = Figure id name cap img 

_articleType _ = inner . queryT [jq| #articleHead h2 |]

_doi :: PaperReader -> Cursor -> T.Text
_doi _ = decodeUrl . head . (attribute "content") . head . queryT [jq| meta[name='dc.Identifier'][scheme='doi'] |]

_journal _ = inner . queryT [jq| #qsProduct span |] --  &/ element "span"

_year _ = fmap (read . T.unpack . bottom 4) . headm . getMeta "dc.Date"
  where
    bottom n = T.reverse . T.take n . T.reverse

_authors _ = getMeta "dc.Creator"

_volume _ = inner . queryT [jq|span.citation_volume|]

_publisher _ _ = Just "ACS"


--ToDo: these are not perfect yet.
_mainHtmlL :: PaperReader -> Cursor -> Maybe PaperMainText
_mainHtmlL r c =
  let
    level = ((supported r) r "")  -- stub
  in
    fmap FlatHtml $ render . map replaceLinks . removeH2Abstract . removeQueries removelist .
    map node . queryT [jq| #articleBody|] $ c

-- Ad hoc.
-- I should generalize this kind of deletion. (In this case, any occurence of <h2>Abstract</h2>)
removeH2Abstract :: [Node] -> [Node]
removeH2Abstract ns = (removeDepth f 1 (head ns)):tail ns
  where
    f (NodeElement (Element "h2" _ [NodeContent "Abstract"])) = True
    f _ = False

removelist = ["div#abstractBox","#references","div.figure","ul.anchors","hr"]

_mainHtmlA :: PaperReader -> Cursor -> Maybe PaperMainText
_mainHtmlA _ c =
  let
    mkHtml :: (Text,Node) -> Tree (Text,Text)
    mkHtml (title,node) = Node (title,fromMaybe "" $ render $ map replaceLinks $ removeQueries removelist [node]) []
  in do
    body <- headMay $ queryT [jq| #articleBody |] c
    let sections = (map removeSectionHeader $ (map node . query ".NLM_sec_level_1") body) ++
                  [("",(node . head . query ".NLM_back") body)]
    return $ Structured (Node ("","") (map mkHtml sections))

replaceLinks = mapAttr [] replaceHref . mapAttr [] replaceImgSrc

mkFileNameTxt = T.pack . mkFileName . T.unpack

replaceImgSrc :: M.Map Name Text -> M.Map Name Text
replaceImgSrc attr =
  let
    f name value
      | name == "src" =
          if T.head value == '/' then (T.pack resourceRootUrl) `T.append` (mkFileNameTxt (fullurl value))
          else value
      | otherwise = value
    fullurl path = "http://pubs.acs.org" `T.append` path
  in
    M.mapWithKey f attr

replaceHref :: M.Map Name Text -> M.Map Name Text
replaceHref attr =
  let
    f name value
      | name == "href" =
          if T.head value == '/' then "http://pubs.acs.org" `T.append` value
          else value
      | otherwise = value
  in
    M.mapWithKey f attr


mapAttr :: [Text] -> (M.Map Name Text -> M.Map Name Text) -> Node -> Node
mapAttr tags func n@(NodeElement (Element name attr cs))
   = NodeElement (Element name (func attr) (map (mapAttr tags func) cs))
 -- | otherwise = n
mapAttr _ _ n = n

-- removeSectionHeader cur = ("",head $ removeTags ["h2"] [node cur])

removeSectionHeader :: Node -> (T.Text,Node)
removeSectionHeader nd =
  let
    NodeElement (Element en ea ns) = nd
    getContent :: [Node] -> T.Text
    getContent = T.concat . catMaybes . map f
      where
        f (NodeContent t) = Just t
        f _ = Nothing
    txt = maybe "" fromJust $ find isJust $ map g ns
    g n = case n of
            NodeElement (Element "h2" _ cs) -> Just (getContent cs)
            _ -> Nothing
  in
    (txt, head $ removeTags ["h2"] [nd])

getDoiFromHref :: Monad m => [String] -> m (Maybe String)
getDoiFromHref s = do
  return $ find (not . null) (map f s)
  where
    f :: String -> String
    f s = either (\x->"") id $ parse p "" s
    p :: Parsec String () String
    p = do
      manyTill anyChar (try (string "key="))
      try (string "10.")
      s <- many1 (noneOf "?")
      return $ "10." ++ s


pages :: Cursor -> (Maybe T.Text,Maybe T.Text)
pages c = parsePages ss
  where
    ss :: T.Text
    ss = last $ (c $// byId "citation" >=> child >=> content)

parsePages :: T.Text -> (Maybe T.Text,Maybe T.Text)
parsePages input = case parse p "pages" (T.unpack input) of
                      Left err -> (Nothing,Nothing)
                      Right (x,y) -> (Just x, y)
  where
    p :: Parsec String () (T.Text,Maybe T.Text)
    p = do
      manyTill anyChar (try (string "pp ") <|> try (string "p "))
      fr <- many1 digit
      anyChar
      to <- optionMaybe $ many1 digit
      return (T.pack fr,fmap T.pack to)

