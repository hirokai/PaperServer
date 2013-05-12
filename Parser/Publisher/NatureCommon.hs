--
-- NatureCommon.hs
--


{-# LANGUAGE DoAndIfThenElse #-}

module Parser.Publisher.NatureCommon where

import Parser.Import
-- import Safe
-- import Data.Maybe
import Text.XML.Cursor
import Parser.Utils (inner,(<||>))
import qualified Data.Text as T

import Control.Applicative ((<|>))

_doi :: ReaderElement' Text
_title, _journal, _volume, _pageFrom, _pageTo
	:: ReaderElement' (Maybe Text)
_publisher :: ReaderElement (Maybe Text)
_year :: ReaderElement' (Maybe Int)
_authors :: ReaderElement' [T.Text]

_publisher _ _ _ = Just "NPG"

drop' :: Int -> T.Text -> Maybe T.Text
drop' n s | T.length s >= n = Just (T.drop n s)
          | otherwise = Nothing

_doi _ c = fromMaybe "" $ (headMay $ getMeta "citation_doi" c) >>= drop' 4

_title _ c = (inner . queryT [jq| #atl |]) c <|> (headm . getMeta "dc.title") c

_journal _ = headMay . getMeta "prism.publicationName"

_volume _ = headMay . getMeta "prism.volume"

_pageFrom _ = headMay . getMeta "prism.startingPage"

_pageTo _ = headMay . getMeta "prism.endingPage"


-- ToDo: or: _year _ = fmap (read . T.unpack . T.take 4) . headm . getMeta "prism.publicationDate"
_year _ cur =
  let
    date = getMeta "dc.date" cur <||> getMeta "DC.date" cur
  in
    case headm date of
                  Just s | T.length s >= 4 -> (Just . read . T.unpack . T.take 4) s
                  _ -> Nothing

_authors _ c = getMeta "dc.creator" c <||> getMeta "DC.creator" c

_articleType _ c = (headm . getMeta "prism.section") c <|> (maybeText . innerText . queryT [jq| h1.page-header |]) c

