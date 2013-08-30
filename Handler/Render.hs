{-# LANGUAGE DoAndIfThenElse,DeriveDataTypeable,TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Handler.Render where

import Import

import Control.Lens

import Text.Blaze.Html.Renderer.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO 

import Text.Blaze.Html
import Text.HTML.SanitizeXSS (sanitize)

import Yesod.Auth

import System.Directory (doesFileExist,removeFile)

import qualified Parser.Paper as P
import Model.PaperMongo
import Model.PaperP

import Handler.Utils
import Handler.Widget

-- for FormatC
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL8 
import qualified Data.ByteString.Lazy.Char8 as BL8 
import Data.Data 
import Data.Generics

data RenderFormat = FormatA | FormatB | FormatC |
                    FormatATablet | FormatBTablet |
                    FormatAMobile | FormatBMobile
      deriving (Show,Eq,Enum,Bounded)

-- Render paper.
renderPaper :: PaperId -> RenderFormat -> Handler TypedContent
renderPaper paperId format = do
  email <- requireAuthId'
  res <-  getPaperDB email paperId
  case res of
    Just paper -> do  -- ToDo: Take only pid to avoid DB overhead.
      let path = renderedCachePath format paperId
--      exist <- liftIO $ doesFileExist path
      exist <- return False
      if exist then
        sendFile typeHtml path
      else do
        let pp = paperToPaperP paper
        saveFormattedCache format paperId pp
        ex <- liftIO $ doesFileExist path
        if ex then
          sendFile typeHtml path
        else
          notFound
    Nothing ->
      notFound

data PaperMastache = PaperMastache {
  citation :: Citation2
  , mcitHtml :: Text
  , abstract :: Text
  , mainHtml :: Text
  , title :: Text
  , paperId :: Text
} deriving (Data, Typeable)


-- Citation with no maybe values
data Citation2 = Citation2 {
    cdoi :: Text,
    curl :: Text,
    ctitle :: Text,
    cjournal :: Text,
    cyear :: Int,
    cvolume :: Text,
    cpageFrom :: Text,
    cpageTo :: Text,
    cauthors :: [Text],
    cpublisher :: Text,
    ctype :: Text
} deriving (Data, Typeable)

fromCit :: P.Citation -> Citation2
fromCit (P.Citation doi url title journal year volume pageFrom pageTo authors publisher _type)
 = Citation2
    (fromMaybe "" doi)
    (fromMaybe "" url)
    (fromMaybe "" title)
    (fromMaybe "" journal)
    (fromMaybe 0 year)
    (fromMaybe "" volume)
    (fromMaybe "" pageFrom)
    (fromMaybe "" pageTo)
    (authors)
    (fromMaybe "" publisher)
    (fromMaybe "" _type)

toMastacheP :: PaperId -> PaperP -> PaperMastache
toMastacheP pid p =
  let
    title = fromMaybe "N/A" $ p^.P.paperCitation^.P.citationTitle
    authors = p^.P.paperCitation^.P.citationAuthors
    mainHtml = case p^.P.paperMainHtml of
                  Just (P.FlatHtml t) -> t
                  Just _ -> "Stub: not supported structured text"
                  Nothing -> "(Not available)"
    abstract = fromMaybe "(No abstract)" $ p^.P.paperAbstract
    cit = p^.P.paperCitation -- emptyCitation{citationTitle=Just title,citationAuthors=authors}
    cittxt = T.concat ["<i>",fromMaybe "" $ cit^.P.citationJournal, "</i>",
                        maybe "" (\v -> T.concat [", <b>", v, "</b>"]) (cit^.P.citationVolume),
                        maybe "" (\p -> T.append ", " p) (cit^.P.citationPageFrom),
                        maybe "" (\p -> T.append "-" p) (cit^.P.citationPageTo),
                        maybe "" (\y -> T.concat [" (",T.pack $ show y,")"]) (cit^.P.citationYear)]
  in
    PaperMastache (fromCit cit) cittxt abstract mainHtml title (toPathPiece pid)

renderMastache :: FilePath -> PaperId -> PaperP -> IO (PageContent (Route App))
renderMastache file pid pp = do
  template <- B.readFile file
  let inf = toMastacheP pid pp
  res <- hastacheStr (defaultConfig{muEscapeFunc=emptyEscape}) template
        (mkGenericContext inf) 
  return $ PageContent (preEscapedToHtml ("Test title"::Text))
                     (\_ -> preEscapedToHtml ("Test head"::Text))
                     (\_ -> preEscapedToHtml $ decodeUtf8 $ BL8.toStrict $ res)


saveFormattedCache :: RenderFormat -> PaperId -> PaperP -> Handler ()
saveFormattedCache format paperId pp = do
  render <- getUrlRender
  let
    paper = paperPToPaper pp
    cit = pp^.P.paperCitation
    cit' = paperCitation paper
    refs = pp^.P.paperReferences
    refs' = paperReferences paper
    figures = pp^.P.paperFigures
    mabstract = pp^.P.paperAbstract
    mmainHtml = fmap renderStructured $ pp^.P.paperMainHtml
    parser = pp^.P.paperParserInfo
    ctype = cit^.P.citationType
  PageContent title head body
    <- case format of
            FormatA -> widgetToPageContent $(widgetFile "format_a")
            FormatB -> widgetToPageContent $(widgetFile "format_b")
            FormatC -> liftIO $ renderMastache "templates/format_c.mastache.html" paperId pp
            FormatAMobile -> widgetToPageContent $(widgetFile "format_a_mobile")
            _ -> widgetToPageContent $(widgetFile "format_b")

  liftIO $ TLIO.writeFile (renderedCachePath format paperId) (renderHtml (body (\_ _ -> "")))
  return ()

renderedCachePath :: RenderFormat -> PaperId -> String
renderedCachePath format pid = appRootFolder ++ "data/"++show format++"/" ++ T.unpack (toPathPiece pid)

deleteAllRenderedCache :: PaperId -> Handler ()
deleteAllRenderedCache pid = do
  forM_ (enumFrom minBound) $ \format -> do
    let path = renderedCachePath format pid
    ex <- liftIO $ doesFileExist path
    if ex then
      liftIO $ removeFile path
    else
      return ()
  

