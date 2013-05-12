-- Handler.Paper


{-# LANGUAGE QuasiQuotes,DoAndIfThenElse #-}

module Handler.Paper where

import Import

-- import Yesod.Auth -- (requireAuthId')

import qualified Data.List as L
-- import Data.Text (Text) 
import qualified Data.Text as T
-- import Data.Maybe
-- import Text.Blaze.Internal
-- import System.IO.Error
import Safe

-- import qualified Data.Map as M

-- import Data.Text.Encoding
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Ae
-- import Text.HTML.DOM (parseLBS)
-- import Text.XML (Document)
import qualified Model.PaperReader as PR
-- import Control.Monad (forM)
import Text.Blaze.Html.Renderer.Text

import Handler.Widget
import Handler.Form
import Handler.Utils
import Handler.Render

-- import Model.PaperReader
import Model.PaperP
import Model.PaperMongo

import Control.Lens hiding ((.=))

import qualified Parser.Paper as P
import Parser.PaperReader (readerFromHtml,readerFromUrl,parseHtml)
import Parser.PaperReaderTypes

import Data.Time.Clock

-- to use Html into forms
import Yesod.Form.Nic (YesodNic)
instance YesodNic App

--
-- Handlers for adding a paper.
--


--FIXME: All runDB functions should be changed to raw mongo.

postAddUrlR :: Handler TypedContent
postAddUrlR = do
  email <- requireAuthId'
  murl <- lookupPostParam "url"
  $(logInfo) $ T.concat ["postAddUrlR from: ",email,":",fromMaybe "(No url)" murl]
  let
    mreader = murl >>= readerFromUrl
    json = object ["supported".=isJust mreader]
  time <- liftIO $ getCurrentTime
  let history = History Nothing HAVisitOriginal time (User email Nothing Nothing) Nothing
  _ <- runDB $ insertUserH email history
  return $ toTypedContent json


optionsAddPaperR :: Handler RepPlain
optionsAddPaperR = do
    -- ToDo: Add other sites.
    setHeader "Access-Control-Allow-Origin" "http://pubs.acs.org"
    setHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"
    return $ RepPlain $ toContent ("" :: Text)
    
-- |Entry point for adding a paper.
-- |This uses two POST params, url and html.
postAddPaperR :: Handler TypedContent
postAddPaperR = do
  email <- requireAuthId'
  $(logInfo) $ T.append "postAddPaperR from: " email
  [murl,mhtml] <- mapM lookupPostParam ["url","html"]
  -- $(logInfo) $ T.concat [fromMaybe "Nothing" url, " ", fromMaybe "Nothing" html]
  case (murl,mhtml) of
    (Just u,Just h) -> addIfNotExist email u h
    _ -> return $ toTypedContent $ object ["success" .= False,"message" .= ("Params url and html are needed." :: Text)]

-- ToDo: Currently this uses URL for identity, but there may be a better choice.
-- DOI is not good, because the abstract and fulltext (SAbstract and SFullText) have the same DOI.
addIfNotExist :: Text -> Text -> Text -> Handler TypedContent
addIfNotExist email url html = do
  email <- requireAuthId'
  res <- getPaperByUrl email url
  json <- case res of
    Just (pid,paper) ->
      return $ object ["success" .= False, "message" .= ("Already exists" :: Text),
                      "doi" .= (paperDoi paper), "id" .= toPathPiece pid, "summary" .= object (paperSummary pid paper)]
    Nothing -> doAdd email url html
  return $ toTypedContent json

doAdd :: Text -> Text -> Text -> Handler Value
doAdd email url html = do
  let mreader = readerFromHtml url html
  -- liftIO $ TIO.writeFile (CachePath email url) html
  -- runDB $ deleteWhereUser email [PaperUrl ==. url] -- In case paper exists but no html is found.
  mrep <- case mreader of
                Just r -> do
                  $(logInfo) $ T.append "Reader selected: " ((\a -> (readerName a) a) r)
                  parseAndAdd email r url html
                Nothing -> do
                  $(logWarn) "No Reader was found."
                  return (Nothing,Nothing)
  return $ case mrep of
             (Just rep,Just pid) -> object [ "success" .= True, "summary" .= object (paperSummary pid rep)]
             _ -> object [ "success" .= False , "message" .= ("No reader was found." :: String)]

parseAndAdd :: Text -> PaperReader -> Url -> Text -> Handler (Maybe Paper,Maybe PaperId)
parseAndAdd email _ url html = do
  mnewpp <- liftIO $ parseHtml url html
  let mnewp = fmap paperPToPaper mnewpp  -- Take out Paper from PaperP
  $(logInfo) $ "Paper got from parser"
  case mnewp of
    Just newp -> do
      time <- liftIO $ getCurrentTime
      pid <- runDB $ insertUser email newp{paperTimeAdded=time}
      $(logInfo) "Paper added to DB"
      saveFormattedCache FormatB pid (fromJust mnewpp)   -- Stub: Also change reparse.
      time <- liftIO $ getCurrentTime
      let history = History (Just pid) HACreate time (User email Nothing Nothing) Nothing
      _ <- runDB $ insertUserH email history
      return (mnewp,Just pid)
    Nothing -> do
      $(logWarn) $ "Parsing failure."
      return (Nothing,Nothing)

--
-- Handlers for checking if the specified paper is supported.
--

getSupportedPaperR :: Handler TypedContent
getSupportedPaperR = supportedPaperR lookupGetParam

postSupportedPaperR :: Handler TypedContent
postSupportedPaperR = supportedPaperR lookupPostParam

type LookupFunc = Text -> Handler (Maybe Text)

supportedPaperR :: LookupFunc -> Handler TypedContent
supportedPaperR f = do
  murl <- f "url"
  let
    mreader = murl >>= readerFromUrl
    pubname = maybe "N/A" (\r -> (readerName r) r) mreader
    pub = ["publisher" .= pubname]
    json = object (["supported" .= isJust mreader] ++ pub)
  return $ toTypedContent json


--
-- Handlers for modifying (removing, reparsing, changing tags) papers.
--

formHandlerWithIdtext form func = do
  ((result, widget), enctype) <- runFormPost form
  json <- case result of
            FormSuccess idtext -> do
              let ids = splitIdText $ Just idtext
              case ids of
                  [] -> return $ object (["success" .= False, "message" .= ("ID is invalid."::String)])
                  xs -> func xs
            FormMissing -> return $ jsonFormError
            FormFailure _ -> return $ jsonFormError
  return $ toTypedContent json

formHandlerWithIdtextAndParams form func = do
  ((result, widget), enctype) <- runFormPost form
  json <- case result of
            FormSuccess idtext -> do
              let ids = splitIdText $ Just idtext
              case ids of
                  [] -> return $ object (["success" .= False, "message" .= ("ID is invalid."::String)])
                  xs -> do
                    partext <- lookupPostParam "params"
                    let params = splitParamText partext
                    func xs params
            FormMissing -> return $ jsonFormError
            FormFailure s -> do
              $(logError) $ T.pack $ show s
              return $ jsonFormError
  return $ toTypedContent json

jsonFormError :: Value
jsonFormError = object ["success" .= False, "message" .= ("Error while processing the form."::String)]

postRemovePapersR :: Handler TypedContent
postRemovePapersR = formHandlerWithIdtext onetimeTokenF doRemove

postReparsePapersR :: Handler TypedContent
postReparsePapersR = formHandlerWithIdtext onetimeTokenF doReparse

getReparsePaperR :: Handler TypedContent 
getReparsePaperR = do  -- caution: this is for a single pid, different from postReparsePapersR.
  mpid <- lookupGetParam "id"
  let mpidstr = mpid >>= fromPathPiece
  memail <- requireAuthId'
  json <- case mpidstr of
    Just pid -> do
      doReparse [pid]
    Nothing -> do
      return $ object ["success" .= False, "message" .= ("No valid paper ID found."::String)] 
  return $ toTypedContent json

formHandlerWithIdTagText form func = do
  ((result, widget), enctype) <- runFormPost form
  liftIO $ print result
  json <- case result of
            FormSuccess (idtext,tagtext) -> do
              let ids = splitIdText $ Just idtext
              let tags = splitParamText $ Just tagtext
              case (ids,tags) of
                  ([],_) -> return $ object (["success" .= False, "message" .= ("ID is invalid."::String)])
                  (_,[]) -> return $ object (["success" .= False, "message" .= ("Tag is invalid."::String)])
                  (is,ts) -> func is ts 
            FormMissing -> return jsonFormError
            FormFailure _ -> return jsonFormError
  return $ toTypedContent json

postAddTagsR = formHandlerWithIdtextAndParams onetimeTokenF doAddTags
postRemoveTagsR = formHandlerWithIdtextAndParams onetimeTokenF doRemoveTags

postReplaceTagsR :: Handler TypedContent
postReplaceTagsR = formHandlerWithIdtextAndParams onetimeTokenF doReplaceTags
  

-- Actions
-- These are called from post***R functions.
-- These should not be directly called from a client to avoid CSRF.

doReparse :: [PaperId] -> Handler Value
doReparse pids = do
  email <- requireAuthId'
  --FIXME: Support multi paper.
  ps <- getPapersByIds email pids
  if null ps then
    return $ object ["success" .= False
                     , "action" .= ("Reparse"::String)
                     , "message" .= ("Papers with these IDs were not found"::String)
                     , "id" .= map toPathPiece pids]
  else do
    json <- forM (zip pids ps) $ \(pid,p) -> do
      mrep <- liftIO $ parseHtml (paperUrl p) (paperOriginalHtml p)
      case mrep of
        Just rep -> do
          runDB $ replaceUser email pid (paperPToPaper rep)
          deleteAllRenderedCache pid
          return $ object ["success" .= True,
                       "action" .= ("Reparse"::String),
                       "ids" .= map toPathPiece pids,
                       "citation" .= (rep^.P.paperCitation)
                       ]
        Nothing ->
          return $ object ["success" .= False, "message" .= ("Original HTML cache is missing (or you just added a citation, not full text.).":: String)]
    return $ Ae.toJSON json

doRemove :: [PaperId] -> Handler Value
doRemove pids = do
  email <- requireAuthId'
  runDB $ deleteWhereUser email [PaperId <-. pids]
  time <- liftIO $ getCurrentTime
  let obj = map f pids
  _ <- forM pids $ \pid -> do
    let history = History (Just pid) HARemove time (User email Nothing Nothing) Nothing
    runDB $ insertUserH email history
  return $ toJSON obj
  where
    f :: PaperId -> Value
    f pid = object ["success" .= True, "action" .= ("Remove"::String), "id" .= toPathPiece pid]


-- ToDo: Rewrite these with updateWhere. Current version is kind of verbose.
-- I tried that once by updateWhere [PaperTags +=. tags],
-- but it caused a runtime error somehow.

doAddTags :: [PaperId] -> [Text] -> Handler Value
doAddTags ids tags = do
  liftIO $ print (tags,ids)
  email <- requireAuthId'
  -- let tags = filter (not . T.null) atags
  updatedtags <- forM ids $ \pid -> do
    mpaper <- getPaperDB email pid
    case mpaper of
      Just paper -> do
        let newtags = L.union (paperTags paper) tags
        let newpaper = paper{paperTags=newtags}
        updatePaperDB pid newpaper
        return newtags
      Nothing ->
        return []
  let html = case (headMay ids,headMay updatedtags) of
            (Just pid,Just tags) -> paperTagsHtml pid tags
            (Nothing,Nothing) -> ""
  return $ object ["success" .= True,"replaceHtml" .= (renderHtml html)]
  
doRemoveTags :: [PaperId] -> [Text] -> Handler Value
doRemoveTags ids tags = do
  email <- requireAuthId'
  liftIO $ print (tags,ids)
  updatedtags <- forM ids $ \pid -> do
    paper <- getPaperDB404 email pid
    let oldtags = paperTags paper
    let removed = filter (`elem` oldtags) tags
    let newtags = oldtags L.\\ removed
    let newpaper = paper{paperTags=newtags}
    updatePaperDB pid newpaper
    return newtags
  let html = case (headMay ids,headMay updatedtags) of
            (Just pid,Just tags) -> paperTagsHtml pid tags
            (Nothing,Nothing) -> ""
  return $ object ["success" .= True]

doReplaceTags :: [PaperId] -> [Text] -> Handler Value
doReplaceTags ids tags = do
  runDB $ updateWhere [PaperId <-. ids] [PaperTags =. tags]
  return $ object ["success" .= True, "ids" .= map toPathPiece ids]

postExportCitationR :: Handler RepPlain
postExportCitationR = do
  email <- requireAuthId'
  idtext <- lookupPostParam "id"
  let ids = splitIdText idtext
  ps <- getPapersByIds email ids
  liftIO $ print ids
  sendResponse $ RepPlain $ toContent $ T.intercalate "\n" (map PR.paperRIS ps)

postPaperRefsR = getPaperRefsR

getPaperRefsR :: PaperId -> Handler TypedContent
getPaperRefsR pid = do
  email <- requireAuthId'
  paper <- getPaperDB404 email pid
  if paperUserEmail paper == Just email then do
    let refs = paperReferences paper
    return $ toTypedContent $ Ae.object ["refs" .= ("NA"::String)] -- Stub!!! ["refs" .= (map Ae.toJSON refs)]
  else
    notFound


getPaperInfoAjaxR :: PaperId -> Handler RepHtml
getPaperInfoAjaxR pid = do
  email <- requireAuthId'
  paper <- getPaperDB404 email pid
  nowrapLayout (paperInfoAjax paper)

getPaperCitationR :: PaperId -> Handler RepPlain
getPaperCitationR pid = do
  email <- requireAuthId'
  paper <- getPaperDB404 email pid
  if paperUserEmail paper == Just email then do
    sendResponse $ RepPlain $ toContent $ PR.paperRIS paper
  else
    notFound

