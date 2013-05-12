{-# LANGUAGE DoAndIfThenElse #-}

module Handler.Render where

import Import

-- import Data.Maybe
-- import Control.Monad
import Control.Lens

import Text.Blaze.Html.Renderer.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO 

import Text.Blaze.Html
import Text.HTML.SanitizeXSS (sanitize)

import Yesod.Auth

-- import Import

import System.Directory (doesFileExist,removeFile)

import qualified Parser.Paper as P
-- import Model
import Model.PaperMongo
import Model.PaperP

import Handler.Utils
import Handler.Widget


data RenderFormat = FormatA | FormatB |
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
      exist <- liftIO $ doesFileExist path
      if exist then
        sendFile typeHtml path
      else do
        let pp = paperToPaperP paper
        saveFormattedCache format paperId pp
        sendFile typeHtml path
    Nothing ->
      notFound

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
    <- widgetToPageContent $
          case format of
            FormatA -> $(widgetFile "format_a")
            FormatB -> $(widgetFile "format_b")
            FormatAMobile -> $(widgetFile "format_a_mobile")
            _ -> $(widgetFile "format_b")

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
  

