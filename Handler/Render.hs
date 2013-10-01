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
import Model.PaperMongo hiding (toStrict)
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
import qualified Parser.Lens as L
import Data.Default

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
  , parser :: Text
  , figs :: [P.Figure]
  , refs :: [Reference2]
  , availability_text :: Text
} deriving (Data, Typeable)

deriving instance Data P.Figure
deriving instance Typeable P.Figure

deriving instance Data Reference2
deriving instance Typeable Reference2

deriving instance Data P.Citation
deriving instance Typeable P.Citation

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

instance Default Citation2 where
  def = fromCit def

-- Reference with no maybe values
data Reference2 = Reference2 {
  refId :: Text,
  refName :: Text,
  refCit :: Citation2,
  refText :: Text,
  refUrl :: Url 
}

fromRef :: P.Reference -> Reference2
fromRef (P.Reference id name cit txt url)
  = Reference2
      id
      name
      (maybe def fromCit cit)
      (fromMaybe "" txt)
      (fromMaybe "" url)

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
    cit = p^.L.citation
    title = fromMaybe "N/A" $ cit^.L.title
    authors = p^.L.citation^.L.authors
    mainHtml = case p^.L.mainHtml of
                  Just (P.FlatHtml t) -> t
                  Just _ -> "Stub: not supported structured text"
                  Nothing -> "(Not available)"
    abstract = fromMaybe "(No abstract)" $ p^.L.abstract
    cittxt = T.concat ["<i>",fromMaybe "" $ cit^.L.journal, "</i>",
                        maybe "" (\v -> T.concat [", <b>", v, "</b>"]) (cit^.L.volume),
                        maybe "" (\p -> T.append ", " p) (cit^.L.pageFrom),
                        maybe "" (\p -> T.append "-" p) (cit^.L.pageTo),
                        maybe "" (\y -> T.concat [" (",T.pack $ show y,")"]) (cit^.L.year)]
    parser = fromMaybe "" $ p^.L.parserInfo
    figs = p^.L.figures
    refs = p^.L.references
    f (P.Figure a b c d) = P.Figure a b c (T.pack $ localRes d)
    avail = T.intercalate ";" $ catMaybes [
              if isJust (p^.L.abstract) then Just "abs" else Nothing,
              if isJust (p^.L.mainHtml) then Just "full" else Nothing,
              if null figs then Nothing else Just "figs",
              if null refs then Nothing else Just "refs"
                ]
  in
    PaperMastache
      (fromCit cit) cittxt abstract mainHtml title (toPathPiece pid)
      parser (map f figs) (map fromRef refs)
      avail

renderMastache :: FilePath -> PaperId -> PaperP -> IO (PageContent (Route App))
renderMastache file pid pp = do
  template <- B.readFile file
  let inf = toMastacheP pid pp
  res <- hastacheStr (defaultConfig{muEscapeFunc=emptyEscape}) template
        (mkGenericContext inf) 
  return $ PageContent (preEscapedToHtml ("Test title"::Text))
                     (\_ -> preEscapedToHtml ("Test head"::Text))
                     (\_ -> preEscapedToHtml $ decodeUtf8 $ toStrict $ res)

toStrict = B.concat . BL8.toChunks

saveFormattedCache :: RenderFormat -> PaperId -> PaperP -> Handler ()
saveFormattedCache format paperId pp = do
  render <- getUrlRender
  let
    paper = paperPToPaper pp
    cit = pp^.L.citation
    cit' = paperCitation paper
    refs = pp^.L.references
    refs' = paperReferences paper
    figures = pp^.L.figures
    mabstract = pp^.L.abstract
    mmainHtml = fmap renderStructured $ pp^.L.mainHtml
    parser = pp^.L.parserInfo
    ctype = cit^.L.ptype
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
  

