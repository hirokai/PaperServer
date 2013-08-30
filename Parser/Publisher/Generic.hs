{-# LANGUAGE QuasiQuotes #-}


--
-- Parser.Publisher.Generic
--

module Parser.Publisher.Generic (
    genericReader
  ) where

import Prelude
import Parser.Import

import Text.XML
import Text.XML.Cursor as C
import Data.Text.Lazy as TL (toStrict, concat)
import Data.List

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec hiding ((<|>))

import Parser.Utils
import Control.Lens
import Data.Tree
import Control.Applicative
import qualified Data.Map as M
import Settings
import Debug.Trace

genericReader = defaultReader {
  supportedUrl = _supportedUrl
--  , supported = _supported
  , title = anyLevel _title
  , abstract = anyLevel _abstract
  , doi = anyLevel _doi
  , journal = anyLevel _journal
  , volume = anyLevel _volume
  , pageFrom = anyLevel _pageFrom
  , pageTo = anyLevel _pageTo
  , year = anyLevel _year
  , authors = anyLevel _authors
-- , articleType = anyLevel _articleType,
  , publisher = anyLevel _publisher
  , readerName = \_ -> "Other journals (generic reader)"
}

_supportedUrl _ url
  = if any (`T.isInfixOf` url) genericReaderTargets then
      Just SAbstract
    else
      Nothing

_title _  c = (headMay . getMeta "dc.title" $ c) <|> (innert . queryT [jq| #article-title |]) c <|> (innert . queryT [jq| h1 |]) c

_journal _ c = (headMay . getMeta "prism.publicationName" $ c)

_abstract _ c =  (inner . queryT [jq| #abstract |]) c <|> (inner . queryT [jq| .abstract |]) c

_volume _ c = (headMay $ getMeta "prism.volume" c)

_pageFrom _ = headMay . getMeta "prism.startingPage"

_pageTo _ = headMay . getMeta "prism.endingPage"

_doi _ c = fromMaybe "" $ (headMay . getMeta  "dc.identifier" $ c) <|> (headMay . getMeta "DC.Identifier" $ c)

_authors _ c = getMeta  "citation_author" $ c

_year _ c = (headMay $ getMeta  "prism.publicationDate" c) >>= takemt 4 >>= (readMay . T.unpack)

_publisher _ c = headMay . getMeta  "dc.publisher" $ c


