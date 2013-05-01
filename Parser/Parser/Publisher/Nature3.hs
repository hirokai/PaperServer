-----------------------------------------------------------------------------
--
-- Module      :  Model.PaperReader.Nature3
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
-- Nature (old, such as 1999) (or all other formats than supported with other Nature readers?)
--

{-# LANGUAGE DoAndIfThenElse #-}

module Parser.Publisher.Nature3 (
  nature3Reader
) where


import Import
import Data.Maybe
import Text.XML.Cursor
import Parser.Utils
import qualified Data.Text as T

import Control.Applicative ((<|>))

import Parser.Publisher.NatureCommon

nature3Reader = defaultReader {
  supportedUrl = _supportedUrl,
  supported = _supported,
  title = anyLevel _title,
  abstract = absLevel _abstract,
  doi = anyLevel _doi,
  journal = anyLevel _journal,
  volume = anyLevel _volume,
  pageFrom = anyLevel _pageFrom,
  pageTo = anyLevel _pageTo,
  year = anyLevel _year,
  authors = anyLevel _authors,
  articleType = anyLevel _articleType,
  refs = onlyFullL _refs,
--  figs = _figs,
--  toc = _toc,
  mainHtml = onlyFull _mainHtml,
  publisher = _publisher,
  readerName = _readerName
}

_readerName _ = "Nature3"

_supportedUrl _ url = if any  (`T.isPrefixOf` url) ["http://www.nature.com/"] then Just SFullText else Nothing
_supported r url cur =
   if isJust (_supportedUrl r url) && (not . null . getMeta "prism.publicationName") cur
                          && isJust (_abstract r cur)
   then
     Just SFullText
   else
     Nothing

_abstract _ c = (inner $ queryT [jq|p.lead|] c)

-- _mainHtml _ = maybeText . toStrict . toHtml . queryT [jq| #articlebody p |]
_mainHtml _ = fmap FlatHtml . render . rmElem "div" "abs" [] . rmElem "div" "" ["figure-table"] . map node . queryT [jq| #articlebody |]

_refs _ c = case cs1 of
              [] -> map extCit2 (Prelude.zip [1..] cs2)
              _ -> map extCit1 cs1
  where
    cs1 = queryT [jq| #References tr |] c
    cs2 = queryT [jq| #References li |] c

extCit2 :: (Int,Cursor) -> Reference
extCit2 (num,c) = Reference (fromMaybe "N/A" (eid . node $ c))
                      (T.pack $ show num)
                      Nothing
                      (maybeText . innerText . (:[]) $ c)
                      url
  where
    url = do
            raw <- mraw
            return $ if T.head raw == '/' then
              "http://www.nature.com" `T.append` raw
            else
              raw
      where
        mraw = ((\xs -> atMay xs 1) $ queryT [jq| a |] c) >>= headm . attribute "href"

extCit1 :: Cursor -> Reference
extCit1 c = Reference (fromMaybe "N/A" (td1a >>= headm . attribute "name"))
                      (fromMaybe "N/A" (td1a >>= maybeText . innerText . (:[])))
                      Nothing
                      (td2 >>= maybeText . innerText . (:[]))
                      url
  where
    td1a, td2 :: Maybe Cursor
    td1a = headm $ queryT [jq| td[align="right"] a |] c
    td2 = headm $ queryT [jq| td[align="left"] |] c
    url = do
            raw <- mraw
            return $ if T.head raw == '/' then
              "http://www.nature.com" `T.append` raw
            else
              raw
      where
        mraw = td2 >>= headm . queryT [jq| a |] >>= headm . attribute "href"
