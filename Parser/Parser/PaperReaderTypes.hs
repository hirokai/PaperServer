module Parser.PaperReaderTypes where

-- import Import

import Text.XML
import Data.Text (Text) 
import qualified Data.Text as T (pack,unpack,concat,Text,replace,append,intercalate,splitOn,head)
import Data.List as L
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString as B
import Parser.Paper
-- import qualified Paper as P

import Text.XML.Cursor(Cursor,fromDocument)

type ReaderElement a = PaperReader -> SupportLevel -> Cursor -> a

data PaperReader = PaperReader {
  supportedUrl :: PaperReader -> Url -> Maybe SupportLevel,
    -- Return non-Nothing value if this url may be supported.
    -- This is used to inform client if html should be sent to server or not.
    -- It can happen supportedUrl returns true and supported returns false.
    -- Also, you need to check url at supported as well.
--  supportedUrl _ _ = False
  supported :: PaperReader -> Url -> Cursor -> Maybe SupportLevel,
  parsePaper :: PaperReader -> Url -> T.Text -> Document -> IO Paper,

  -- ToDo: IO: To make parsers report progress and error. May change in the future
  doi :: ReaderElement Text,
  abstract, journal, volume, pageFrom, pageTo, articleType, publisher,toc,title
    :: ReaderElement (Maybe Text),
  mainHtml :: ReaderElement (Maybe PaperMainText),
  year :: ReaderElement (Maybe Int),
  authors :: ReaderElement [Text],
  refs :: ReaderElement [Reference],
  figs :: ReaderElement [Figure],
  misc :: ReaderElement B.ByteString,
  sections :: ReaderElement (Maybe SectionInfo),
  readerName :: PaperReader -> Text 
}

-- TODO: Differentiate title in Paper and title in Citation
-- title in Citation should be plain text
-- title in Paper should be real title including superscript, greek letter, by html


-- Make sure this defaultReader covers all fields. Compiler does not catch missing fields.
defaultReader = PaperReader {
  supportedUrl = (\r u -> Nothing),
  supported = (\r url _ -> (supportedUrl r) r url),
  doi = \_ _ _ -> "N/A",
  parsePaper = defParsePaper,
  abstract = parseNothing,
  mainHtml = \_ _ _ -> Nothing,
  journal = parseNothing,
  volume = parseNothing,
  pageFrom = parseNothing,
  pageTo = parseNothing,
  articleType = parseNothing,
  publisher = parseNothing,
  toc = parseNothing,
  title = parseNothing,
  year = parseNothing,
  authors = \_ _ _ -> [],
  refs = \_ _ _ -> [],
  figs = \_ _ _ -> [],
  misc = \_ _ _ -> "",
  sections = \_ _ _ -> Nothing,
  readerName = \_ -> "(default)"
}

defParsePaper :: PaperReader -> Url -> T.Text -> Document -> IO Paper
defParsePaper r url html doc = do
  let c = fromDocument doc
  let ml = (supported r) r url c
  case ml of
    Nothing -> return emptyPaper
    Just l -> do
      let _doi = (doi r) r l c
      let cit = emptyCitation{_citationDoi = Just _doi,_citationUrl=Just url,
                  _citationTitle=(title r) r l c,_citationJournal=(journal r) r l c,
                  _citationYear=(year r) r l c, _citationVolume=(volume r) r l c,
                  _citationPageFrom=(pageFrom r) r l c,_citationPageTo=(pageTo r) r l c,
                  _citationAuthors=(authors r) r l c,_citationPublisher=(publisher r) r l c,
                  _citationType=(articleType r) r l c}
      return emptyPaper{_paperDoi=_doi,_paperHtml=html,_paperUrl=url,_paperCitation=cit,
                        _paperAbstract=(abstract r) r l c, _paperMainHtml=(mainHtml r) r l c,
                        _paperReferences=(refs r) r l c,_paperFigures=(figs r) r l c,
                        _paperToc=(toc r) r l c,_paperMisc=(misc r) r l c,_paperParserInfo = Just $ (readerName r) r
                        , _paperSupportLevel=l}

parseNothing :: ReaderElement (Maybe a)
parseNothing _ _ _ = Nothing

-- This function converts parsing functions with an old format into a new one. (Just ignores SupportLevel argument.)
anyLevel :: (PaperReader -> Cursor -> a) -> ReaderElement a
anyLevel f = (\r l cur -> (f r cur))

absLevel :: (PaperReader -> Cursor -> Maybe a) -> ReaderElement (Maybe a)
absLevel f = (\r l cur ->
  case l of
    SFullText -> (f r cur)
    SAbstract -> (f r cur)
    _ -> Nothing)

onlyFull :: (PaperReader -> Cursor -> Maybe a) -> ReaderElement (Maybe a)
onlyFull f = (\r l cur ->
  case l of
    SFullText -> (f r cur)
    _ -> Nothing)

onlyFullL :: (PaperReader -> Cursor -> [a]) -> ReaderElement [a]
onlyFullL f = (\r l cur ->
  case l of
    SFullText -> (f r cur)
    _ -> [])

-- ToDo: FIXME This quick fix should be removed at some point.
boolToSupp False = Nothing
boolToSupp True = Just SFullText

absolutePath :: T.Text -> T.Text -> T.Text
absolutePath url path
  | T.head path == '/' = (T.intercalate "/" $ take 3 (T.splitOn "/" url)) `T.append` path
  | otherwise = (T.intercalate "/" $ init (T.splitOn "/" url)) `T.append` "/" `T.append` path


doi2url :: String -> String
doi2url doi = "http://dx.doi.org/" ++ doi


emptyMisc :: B.ByteString
emptyMisc = ""
-- emptyMisc = B.concat $ LB.toChunks $ MP.pack (M.fromList [] :: M.Map Int Int)

nodeToText :: Node -> T.Text
nodeToText (NodeElement e)
    = T.concat $ ["<", name, " ", attr, ">"]
   ++ map nodeToText (elementNodes e)
   ++ (if isSingle name then [] else ["</", name,">"])
     where
        name = nameLocalName $ elementName e
        isSingle n = n `elem` [ "area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
        attr = T.pack $ intercalate " " $ map f  (M.toList $ elementAttributes e)
          where f (k,v) = (T.unpack $ nameLocalName k) ++ "=\"" ++(T.unpack v) ++"\""
nodeToText (NodeContent t) = t
nodeToText (NodeInstruction _) = ""
nodeToText (NodeComment _) = ""


decodeUrl :: T.Text -> T.Text
decodeUrl s = T.replace "%2F" "/" s

