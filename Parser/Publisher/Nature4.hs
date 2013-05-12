--
-- Nature4.hs
--

-- e.g. http://www.nature.com/ni/journal/v5/n10/full/ni1112.html

{-# LANGUAGE DoAndIfThenElse #-}

module Parser.Publisher.Nature4 (
  nature4Reader
) where


import Parser.Import
-- import Data.Maybe
import Text.XML.Cursor
import Parser.Utils
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
-- import Control.Applicative ((<|>))

import Parser.Publisher.NatureCommon

nature4Reader = defaultReader {
  supportedUrl = _supportedUrl,
  supported = _supported,
  title = anyLevel _title,
  doi = anyLevel _doi,
  journal = anyLevel _journal,
  volume = anyLevel _volume,
  pageFrom = anyLevel _pageFrom,
  pageTo = anyLevel _pageTo,
  year = anyLevel _year,
  authors = anyLevel _authors,
  articleType = anyLevel _articleType,
--  refs = _refs,
--  figs = _figs,
--  toc = _toc,
  abstract = absLevel _abstract,
  mainHtml = onlyFull _mainHtml,
  publisher = _publisher,
  readerName = _readerName
}


_readerName _ = "Nature4"

_supportedUrl _ url = boolToSupp $ any (`T.isPrefixOf` url) ["http://www.nature.com/"]
_supported r url cur = boolToSupp $ isJust (_supportedUrl r url) && (not . null . getMeta "prism.publicationName") cur
                          && isJust (_abstract r cur)

_abstract _ c = (inner $ queryT [jq| span.articletext > b |] c)

_mainHtml _ c = fmap FlatHtml $ maybeText $ toStrict $ renderNodes $ removeQueries [" > b"] $ map node $ queryT [jq| span.articletext |] c


