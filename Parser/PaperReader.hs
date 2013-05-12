-- PaperReaders.hs
--

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

module Parser.PaperReader (
  parseHtml
  , readerFromUrl
  , readersFromUrl
  , readerFromHtml
  , readersFromHtml
  , readersFromHtmlDoc
) where

import Parser.Import
-- import Data.List
-- import Data.Maybe
-- import qualified Data.Map as M
-- import Safe
import Control.Applicative
-- import Control.Monad (mzero)
import Control.Lens hiding ((.=))

import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)

import qualified Text.HTML.DOM as H
import Text.XML.Cursor
import Text.XML (Document)
-- import Text.XML.Selector (maybeText)

import Data.Tree
import Text.HTML.SanitizeXSS

-- import Data.Aeson

import Parser.Publisher.ACS
import Parser.Publisher.NatureL
import Parser.Publisher.NatureA
import Parser.Publisher.Nature2
import Parser.Publisher.Nature3
import Parser.Publisher.Nature4
import Parser.Publisher.NatureRev
import Parser.Publisher.Elsevier
import Parser.Publisher.PLoSONE
import Parser.Publisher.Science
import Parser.Publisher.Wiley
import Parser.Publisher.PNAS
import Parser.Publisher.AnnualRev
import Parser.Publisher.Rockfeller

-- import System.IO


-- import qualified Data.ByteString.Base64 as B64 (encode)

-- Some functions in this module return a list of possible readers.
-- Some just choose the first one, so the order matters in some cases.
-- ToDo: Organize this system.
-- Especially Nature journals are a bit complicated.
readerList :: [PaperReader]
readerList = [acsAReader,acsLReader,
              natureAReader,natureLReader,
              nature2AReader,nature2LReader, nature2OtherReader,
              natureRevReader,nature3Reader,nature4Reader,
              elsevier1Reader,elsevier2Reader,
              scienceReader,stkeReader,
              wileyReaderC,
              pnasReader,jImmunolReader,
              annualRevReader,
              rupressReader,
              plosReader]

-- This is the primary function.
parseHtml :: Url -> Text -> IO (Maybe Paper)
parseHtml url html = do
  let
    doc = H.parseLBS $ encodeUtf8 $ fromStrict html
    rs = readersFromHtmlDoc url doc
    mr = headMay rs
  case mr of
    Just r -> fmap (Just . sanitizePaper) $ (parsePaper r) r url html doc
    Nothing -> return Nothing


-- Stub: sanitize other things as well.
-- Or maybe I can just sanitize after HTML generation for viewing.
-- And I can keep all my adde JS into a .js file.
sanitizePaper :: Paper -> Paper
sanitizePaper p =
    paperCitation %~ sanitizeCit $
    paperReferences %~ (map sanitizeRef) $
    paperFigures %~ (map sanitizeFig) $
    paperAbstract %~ (fmap sanitize) $
    paperMainHtml %~ (fmap sanitizeMainHtml) $ p

sanitizeMainHtml :: PaperMainText -> PaperMainText
sanitizeMainHtml (FlatHtml html) = FlatHtml (sanitize html)
sanitizeMainHtml (Structured n) = Structured $ sanitizeNode n
  where
    sanitizeNode (Node (tag,txt) ns) = Node (sanitize tag, sanitize txt) (map sanitizeNode ns)

sanitizeRef :: Reference -> Reference
sanitizeRef (Reference id name mcit mtxt murl)
  = Reference (sanitize id) (sanitize name) (sanitizeCit <$> mcit) (sanitize <$> mtxt) (sanitize <$> murl)

sanitizeCit :: Citation -> Citation
sanitizeCit (Citation doi url title journal year vol from to auth pub typ)
  = Citation (sanitize <$> doi) (sanitize <$> url) (sanitize <$> title)
         (sanitize <$> journal) year (sanitize <$> vol) (sanitize <$> from) (sanitize <$> to) (map sanitize auth)
         (sanitize <$> pub)  (sanitize <$> typ)

sanitizeFig :: Figure -> Figure
sanitizeFig (Figure id name annot img) = (Figure (sanitize id) (sanitize name) (sanitize annot) (sanitize img))




readerFromUrl :: Url -> Maybe PaperReader
readerFromUrl url = headMay $ readersFromUrl url

readersFromUrl :: Url -> [PaperReader]
readersFromUrl url = filter (\r -> isJust $ (supportedUrl r) r url) readerList

readerFromHtml :: Url -> T.Text -> Maybe PaperReader
readerFromHtml url html = headMay $  readersFromHtml url html

readersFromHtml :: Url -> T.Text -> [PaperReader]
readersFromHtml url html = filter (\r -> isJust $ (supported r) r url cur) readerList
  where
    doc = H.parseLBS $ encodeUtf8 $ fromStrict html
    cur = fromDocument doc

readersFromHtmlDoc :: Url -> Document -> [PaperReader]
readersFromHtmlDoc url doc = filter (\r -> isJust $ (supported r) r url cur) readerList
  where
    cur = fromDocument doc

