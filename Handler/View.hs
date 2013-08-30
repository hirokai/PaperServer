module Handler.View where

import Import
import Data.Time.Clock

import Model.Epub
import Handler.Utils
import Model.PaperP
import Handler.Render
import Model.PaperMongo (getRawHtmlById)

import Text.Blaze.Html (preEscapedToHtml)

--
-- Handlers for sending / showing the paper. 
--

getPaperRa :: PaperId -> Handler TypedContent
getPaperRa paperId = renderPaper paperId FormatA

getPaperRb :: PaperId -> Handler TypedContent
getPaperRb paperId = renderPaper paperId FormatB

getPaperRc :: PaperId -> Handler TypedContent
getPaperRc paperId = renderPaper paperId FormatC

getPaperTabletRa :: PaperId -> Handler TypedContent
getPaperTabletRa paperId = renderPaper paperId FormatATablet

getPaperTabletRb :: PaperId -> Handler TypedContent
getPaperTabletRb paperId = renderPaper paperId FormatBTablet

getMobilePaperRa :: PaperId -> Handler TypedContent
getMobilePaperRa paperId = renderPaper paperId FormatAMobile

getMobilePaperRb :: PaperId -> Handler TypedContent
getMobilePaperRb paperId = renderPaper paperId FormatBMobile

getDoiPaperR :: Handler TypedContent
getDoiPaperR = notFound
{-
getDoiPaperR :: String -> String -> Handler TypedContent
getDoiPaperR pub doc = do
  email <- requireAuthId'
  let doi = T.pack (pub ++ "/" ++ doc)
  -- ToDo: support different versions for the same DOI.
  p <- getPaperByFilter email ["doi" =: doi]
  let pp = case res of
                ((Entity id p):_) -> Just (p,id)
                _ -> Nothing
  let (orig,paperId) = fromJust pp
  getPaperRb paperId
-}
getRawHtmlR :: PaperId -> Handler TypedContent
getRawHtmlR paperId = do
  email <- requireAuthId'
  mhtml <- getRawHtmlById email paperId
  case mhtml of
    Just html -> return $ toTypedContent $ preEscapedToHtml html
    Nothing -> notFound

getEpubPaperR :: PaperId -> Handler ()
getEpubPaperR pid = do
  email <- requireAuthId'
  paper <- runDB $ get404 pid
  if Just email == paperUserEmail paper then do
    let pp = paperToPaperP paper
    path <- epubFromPaper pid pp
    sendFile "application/epub+zip" path
  else
    notFound

-- history data is a separate table from paper.
doUpdateVisitHistory :: PaperId -> Paper -> Handler ()
doUpdateVisitHistory pid p = do
  time <- liftIO $ getCurrentTime
  case paperUserEmail p of
    Just email -> do
      let history = History (Just pid) HAView time (User email Nothing Nothing) Nothing
      _ <- runDB $ insert history
      return ()
    Nothing -> return ()

