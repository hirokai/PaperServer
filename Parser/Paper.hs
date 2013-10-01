{-# LANGUAGE TemplateHaskell, ImplicitPrelude,DeriveDataTypeable #-}
--
-- Paper
--
-- Definition of Paper and related data types, as well as JSON import and export of them.
--

module Parser.Paper where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Char (toLower)

import Control.Lens hiding ((.=))
import Data.Tree

import Data.Aeson
import Data.Aeson.TH

import Text.XML.Selector (maybeText)

import Data.Typeable
import Data.Default

type Url = Text


-- Don't change field names of data types.
-- deriveJSON and makeLenses rely on current names.

data Reference = Reference {
    _referenceRefId :: Text,
    _referenceRefName :: Text,
    _referenceCit :: Maybe Citation,
    _referenceCitText :: Maybe Text,
    _referenceUrl :: Maybe Url 
    } deriving (Eq,Show)

data Citation = Citation {
    _citationDoi :: Maybe Text,
    _citationUrl :: Maybe Text,
    _citationTitle :: Maybe Text,
    _citationJournal :: Maybe Text,
    _citationYear :: Maybe Int,
    _citationVolume :: Maybe Text,
    _citationPageFrom :: Maybe Text,
    _citationPageTo :: Maybe Text,
    _citationAuthors :: [Text],
    _citationPublisher :: Maybe Text,
    _citationType :: Maybe Text
} deriving (Show,Eq)

data Figure = Figure {
    _figId :: Text,
    _figName :: Text,
    _figAnnot :: Text,
    _figImg :: Url
} deriving (Show,Eq)


data SectionInfo = SectionInfo (Tree Section) deriving Show

data Section = Section {
  _secName :: Text,
  _secId :: Text 
}

instance Show Section where
  show (Section name cur) = T.unpack name -- Stub


data Paper = Paper {
  _paperDoi :: Text,
  _paperUrl :: Text,
  _paperHtml :: Text,
  _paperAbstract :: Maybe Text,
  _paperMainHtml :: Maybe PaperMainText,
  _paperCitation ::Citation,
  _paperReferences :: [Reference],
  _paperFigures ::[Figure],
  _paperResources :: [Resource],
  _paperToc :: Maybe Text,
  _paperTags :: [Text],
  _paperNote :: Maybe Text,
  _paperMisc :: ByteString,     -- Not used now.
  _paperSections :: Maybe SectionInfo,
  _paperParserInfo :: Maybe Text,
  -- _paperUserEmail :: Maybe Text
  _paperSupportLevel :: SupportLevel
} deriving (Show)

data Resource = Resource {
  _resourceId :: Text,
  _resourceUrl :: Text, 
  _resourceType :: Text -- MIME type.
} deriving Show

type SectionTag = Text -- section title
data PaperMainText = Structured (Tree (SectionTag,Text)) | FlatHtml Text deriving Show

instance Default Paper where
  def = Paper "" "" "" Nothing Nothing (def::Citation) [] [] [] Nothing [] Nothing "" Nothing Nothing SUndecidable

emptyPaper :: Paper
emptyPaper = def

instance Default Citation where
  def = Citation Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing

emptyCitation :: Citation
emptyCitation = def

emptyWithUrl :: Text -> Paper
emptyWithUrl url = (def :: Paper){
  _paperCitation=(def :: Citation){_citationUrl=Just url}
  , _paperUrl = url
  }



-- SUndecidable means the reader can't decide the level with the supplied information
-- (i.e. URL for the current implementation)
-- supported can return different values even for the same url,
-- depending on the access environment.
data SupportLevel = SCitation | SAbstract | SFullText | SUndecidable deriving (Show,Eq,Typeable)

showMainText :: PaperMainText -> Text
showMainText (FlatHtml txt) = txt
showMainText (Structured (Node _ cs)) = T.concat (map f cs)  -- stub: now only one-depth
  where
    f (Node (secname,html) _) = T.concat ["<h2 class='section-title'>",secname,"</h2>",html]

--
-- ReferenceManager export
--

paperRIS :: Paper -> T.Text
paperRIS p = T.intercalate "\n" $ catMaybes [
              Just "TY - JOUR"
              , jo
              , ti
              , au
              , vo
              , sp
              , ep
              , py
              , doi
              , url
              , Just "ER - \n"
              ]
  where
    c = _paperCitation p
    ti = fmap ("TI - " `T.append`) (_citationTitle c)
    au = maybeText $ T.intercalate "\n" $ map ("AU - " `T.append`) (_citationAuthors c)
    doi = Just $ "DO - " `T.append` (_paperDoi p)
    jo = fmap ("JO - " `T.append`) (_citationJournal c)
    vo = fmap ("VO - " `T.append`) (_citationVolume c)
    sp = fmap ("SP - " `T.append`) (_citationPageFrom c)
    ep = fmap ("EP - " `T.append`) (_citationPageTo c)
    url = fmap ("UR - " `T.append`) (_citationUrl c)
    py = fmap ("PY - " `T.append`) (fmap (T.pack . show) $ _citationYear c)

