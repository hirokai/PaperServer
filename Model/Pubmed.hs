module Model.Pubmed where

import Import
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL

import Data.Text.Lazy.Encoding
import Network.HTTP.Conduit


import Text.XML as X
import Text.XML.Cursor as C
import Text.XML.Selector
import Text.XML.Selector.TH
import Text.XML.Scraping

import Safe
import Control.Applicative
import Data.Either

pubmedByDOI :: Text -> IO (Maybe PubmedInfo,TL.Text)
pubmedByDOI doi = do
  let url = T.concat ["http://www.ncbi.nlm.nih.gov/pubmed?term=",doi,"&report=xml&format=text"]
  txt <- decodeUtf8 <$> simpleHttp (T.unpack url)
  case parseText def txt of
    Right a -> return $ (Just $ parsePubmedXml (adhocFixDoc a),txt)
    Left _ -> return (Nothing,txt)

pubmedFromXml :: Text -> Maybe PubmedInfo
pubmedFromXml xml =
  case (parseText def . TL.fromStrict) xml of
    Right d ->
      let info = parsePubmedXml d
      in if pmId info == "" then Nothing else Just info
    _ -> Nothing

decodeXml :: Text -> Text
decodeXml txt = T.replace "&lt;" "<" . T.replace "&gt;" ">" $ txt

data PubmedInfo = PubmedInfo {
  pmId :: Text,
  pmTitle :: Maybe Text,
  pmJournal :: Maybe Text,
  pmAuthors :: Maybe [Text],
  pmVolume :: Maybe Text,
  pmYear :: Maybe Int,
  pmPageFrom :: Maybe Text,
  pmPageTo :: Maybe Text
} deriving (Eq,Show)

parsePubmedXml :: Document -> PubmedInfo
parsePubmedXml doc =
  let
    c = fromDocument doc
    title = maybeText $ TL.toStrict $ innerText $ (c $// C.element "ArticleTitle")
    pubmedId = fromMaybe "" $ do
                  h <- headMay $ (c $// C.element "MedlineCitation" &/ C.element "PMID")
                  return $ TL.toStrict $ innerText h
    journal = do
      h <- headMay (c $// C.element "MedlineCitation" &// C.element "Title")
      maybeText $ TL.toStrict $ innerText h
    authors = map mkName $ (c $// C.element "Author")
    pages = mkPages $ TL.toStrict $ innerText $ (c $// C.element "MedlinePgn")
    mkPages :: Text -> (Maybe Text,Maybe Text)
    mkPages txt =
      let
        ts = T.splitOn "-" txt
        pf = atMay ts 0
        pt = atMay ts 1
        f from to = (T.take (T.length from - T.length to) from) `T.append` to
      in
        (pf, liftA2 f pf pt)
    mkName :: Cursor -> Text
    mkName cur =
      let
        ln = TL.toStrict $ innerText $ (cur $.// C.element "LastName")
        fn = TL.toStrict $ innerText $ (cur $.// C.element "ForeName")
      in
        T.concat [fn," ",ln]
    volume = maybeText $ TL.toStrict $ innerText $ (c $// C.element "Volume")
    year = do
      c <- headMay $ (c $// C.element "PubDate" &// C.element "Year")
      readMay $ TL.unpack $ innerText $ c
--  liftIO $ print (title,journal,doc) 
  in
    PubmedInfo pubmedId title journal (if null authors then Nothing else Just authors) volume year (fst pages) (snd pages)


adhocFixDoc :: Document -> Document
adhocFixDoc doc =
  maybe doc mkDoc $ L.find isContent $ elementNodes $ documentRoot doc
  where
    mkDoc (NodeContent txt) =
      case parseText def $ TL.fromStrict txt of
        Right d -> d
        _ -> doc
    isContent (NodeContent _) = True
    isContent _ = False
