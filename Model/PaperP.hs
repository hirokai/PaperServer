--
-- Model.PaperP
-- Interface with Parser package (paperserver-parser)
-- Parser package returns extra info that is not included Paper in the DB.
--

{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Model.PaperP where

import Import hiding (Citation, Reference,Resource,Figure)

import Data.Tree
import qualified Text.Blaze.XHtml5 as H hiding (map)
import qualified Parser.Paper as P  -- So P.Paper means Paper from the parser.
import Data.Aeson.Types (parseMaybe)

import Model
import qualified Data.Text as T

type PaperP = P.Paper

-- Ver 0.3 change Paper is now richer. This also has html text again.
-- This makes the management easier.
-- Tags and userEmail are not in PaperP so just add [] and Nothing.
paperPToPaper :: P.Paper -> Paper
paperPToPaper (P.Paper doi url html abs mainh c ref fig res toc tags note misc sec parser support) =
  Paper
    doi
    url
    html
    abs
    (fmap mkFlat mainh)
    (f c)
    (map g ref)
    (map h fig)
    (map i res)
    toc
    []
    note
    misc
    parser
    support
    avail
    Nothing
    defaultTime
  where
    avail = ResourceAvailability True (isJust abs) (isJust mainh) (not $ null fig) (not $ null ref) (isJust toc)
    f (P.Citation a b c d e f g h i j k) = Citation a b c d e f g h i j k
    g (P.Reference a b c d e) = Reference a b (fmap f c) d e
    h (P.Figure a b c d) = Figure a b c d Unknown
    i (P.Resource rid rurl rtype) = Resource rid rurl rtype rpath
      where
        rpath = imageCachePath url

-- Pure version without no reparsing.
-- For IO version with reparsing from HTML cache, see paperToPaperPIO in Model.PaperReader
-- section info is also missing in Paper for now (because it takes some time to implement tree marshalling)
paperToPaperP :: Paper -> P.Paper
paperToPaperP  (Paper doi url html abs mainh c ref fig res toc tags note misc parser support avail email time) =
  P.Paper
    doi url
    html abs (fmap P.FlatHtml mainh)
    (f c) (map g ref) (map h fig) (map i res)
    toc tags note misc
    Nothing parser support
  where
    f (Citation a b c d e f g h i j k) = P.Citation a b c d e f g h i j k
    g (Reference a b c d e) = P.Reference a b (fmap f c) d e
    h (Figure a b c d e) = P.Figure a b c d
    i (Resource a b c d) = P.Resource a b c


renderStructured :: P.PaperMainText -> Html
renderStructured (P.FlatHtml html) = H.preEscapedToHtml html
renderStructured (P.Structured n) = renderStructured' n

renderStructured' (Node (tag,txt) forest) = do
  H.section $ do
    H.h2 $ H.toHtml tag
    H.preEscapedToHtml txt
    mapM_ renderStructured' forest



{-
type SectionTag = Text -- section title
data PaperMainText = Structured (Tree (SectionTag,Text)) | FlatHtml Text deriving Show


-- SUndecidable means the reader can't decide the level with the supplied information
-- (i.e. URL for the current implementation)
-- supported can return different values even for the same url,
-- depending on the access environment.
data SupportLevel = SCitation | SAbstract | SFullText | SUndecidable deriving (Show,Eq)

showMainText :: PaperMainText -> Text
showMainText (FlatHtml txt) = txt
showMainText (Structured (Node _ cs)) = T.concat (Import.map f cs)  -- stub: now only one-depth
  where
    f (Node (secname,html) _) = T.concat ["<h2 class='section-title'>",secname,"</h2>",html]
-}

-- 
-- JSON in/out This should exactly match that of Parser package, except PaperP is Paper.
--


-- This is used for conversion from PaperP to Paper.
-- ToDo: probably we should keep the structured text in PaperP.
-- You need to write a correct marshalling function. (JSON may have problems as I had before.)
mkFlat :: P.PaperMainText -> Text
mkFlat (P.FlatHtml html) = html
mkFlat (P.Structured n) = mkFlat' n

mkFlat' (Node (tag,txt) forest) = T.append txt (T.concat (map mkFlat' forest))



-- Stub
parseJsonFromClient :: Value -> Maybe P.Paper
parseJsonFromClient (Object v) =
  flip parseMaybe v $ \v -> do
    doi <- v .: "doi"
    url <- v .: "url"
    abs <- v .: "abstract"
    main <- v .: "mainhtml"
    refs <- v .: "references"
    cit <- v .: "citation"
    return $ P.emptyPaper{P._paperDoi=doi
                           ,P._paperUrl=url
                           ,P._paperAbstract=abs
                           , P._paperMainHtml=fmap P.FlatHtml (main :: Maybe Text)
                           , P._paperReferences=map addRefMaybe refs
                           ,P._paperCitation=(addCitMaybe cit)  -- Maybe is not parsed.:
                            -- Pitfall: http://hackage.haskell.org/packages/archive/aeson/0.6.1.0/doc/html/Data-Aeson.html
                          }

    where
      addRefMaybe :: ReferenceNoMaybe -> P.Reference
      addRefMaybe (ReferenceNoMaybe a b c d e) = P.Reference a b (Just $ addCitMaybe c) (Just d) (Just e)
      addCitMaybe :: CitationNoMaybe -> P.Citation
      addCitMaybe (CitationNoMaybe a b c d e f g h i j k) =
        P.Citation (Just a) (Just b) (Just c) (Just d) (Just e) (Just f) (Just g) (Just h) i (Just j) (Just k)

data ReferenceNoMaybe = ReferenceNoMaybe Text Text CitationNoMaybe Text Url

instance FromJSON ReferenceNoMaybe where
  parseJSON (Object v) =
    ReferenceNoMaybe
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "cit"
      <*> v .: "citText"
      <*> v .: "url"

  parseJSON _ = mzero

data CitationNoMaybe = CitationNoMaybe Text Text Text Text Int Text Text Text [Text] Text Text

instance FromJSON CitationNoMaybe where
  parseJSON (Object v) =
    CitationNoMaybe
      <$> v .: "doi"
      <*> v .: "url"
      <*> v .: "title"
      <*> v .: "journal"
      <*> v .: "year"
      <*> v .: "volume"
      <*> v .: "pageFrom"
      <*> v .: "pageTo"
      <*> v .: "authors"
      <*> v .: "publisher"
      <*> v .: "type"

  parseJSON _ = mzero

mergeTwoPapers :: Paper -> Paper -> IO Paper
mergeTwoPapers old new = do
  let ResourceAvailability cit abs full fig ref toc = paperAvailable old
  let ResourceAvailability cit2 abs2 full2 fig2 ref2 toc2 = paperAvailable new
  let p = old{paperAvailable=ResourceAvailability (cit||cit2) (abs||abs2) (full||full2) (fig||fig2) (ref||ref2) (toc||toc2)};
  let p1 = case (cit,cit2) of
              (False,True) -> p{paperCitation=paperCitation new}
              _ -> p
  let p2 = case (abs,abs2) of
              (False,True) -> p1{paperAbstract=paperAbstract new}
              _ -> p1
  let p3 = case (full,full2) of
              (False,True) -> p2{paperMainHtml=paperMainHtml new}
              _ -> p2
  let p4 = case (fig,fig2) of
              (False,True) -> p3{paperFigures=paperFigures new}
              _ -> p3
  let p5 = case (ref,ref2) of
              (False,True) -> p4{paperReferences=paperReferences new}
              _ -> p4
  let p6 = case (toc,toc2) of
              (False,True) -> p5{paperToc=paperToc new}
              _ -> p5
  return p6

