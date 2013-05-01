-- Handler.Paper


{-# LANGUAGE QuasiQuotes,DoAndIfThenElse #-}

module Handler.Paper where

import Import

import Yesod.Auth -- (requireAuthId)

import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe
import Text.Blaze.Internal
import System.IO.Error
import Safe

-- import qualified Data.Map as M

import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Ae
import Text.HTML.DOM (parseLBS)
import Text.XML (Document)
import qualified Model.PaperReader as PR
import Control.Monad (forM)
import Text.Blaze.Html.Renderer.Text

import Handler.Widget
import Handler.Form
import Handler.Utils
import Handler.Render

import Model.PaperReader
import Model.PaperP

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

optionsAddPaperR :: Handler RepPlain
optionsAddPaperR = do
    setHeader "Access-Control-Allow-Origin" "http://pubs.acs.org"
    setHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"
    return $ RepPlain $ toContent ("" :: Text)
    
-- |Entry point for adding a paper.
-- |This uses two POST params, url and html.
postAddPaperR :: Handler RepJson
postAddPaperR = do
  email <- requireAuthId
  $(logInfo) $ T.append "postAddPaperR from: " email
  [url,html] <- mapM lookupPostParam ["url","html"]
  $(logInfo) $ T.concat [fromMaybe "Nothing" url, " ", fromMaybe "Nothing" html]
  case (url,html) of
    (Just u,Just h) -> addIfNotExist email u h
    _ -> jsonToRepJson $ object ["success" .= False,"message" .= ("Params url and html are needed." :: Text)]

-- ToDo: Currently this uses URL for identity, but there may be a better choice.
-- DOI is not good, because the abstract and fulltext (SAbstract and SFullText) have the same DOI.
addIfNotExist :: Text -> Text -> Text -> Handler RepJson
addIfNotExist email url html = do
  email <- requireAuthId
  res <- runDB $ selectListUser email [PaperUrl ==. url, PaperUserEmail ==. Just email] []
  json <- case res of
    (Entity pid paper:_) ->
      return $ object ["success" .= False, "message" .= ("Already exists" :: Text),
                      "doi" .= (paperDoi paper), "id" .= toPathPiece pid, "summary" .= object (paperSummary pid paper)]
    _ -> doAdd email url html
  jsonToRepJson json

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
      pid <- runDB $ insertUser email newp
      $(logInfo) "Paper added to DB"
      saveFormattedCache FormatB pid (fromJust mnewpp)   -- Stub: Also change reparse.
      time <- liftIO $ getCurrentTime
      let history = History pid HACreate time (User email Nothing Nothing)
      _ <- runDB $ insertUserH email history
      return (mnewp,Just pid)
    Nothing -> do
      $(logWarn) $ "Parsing failure."
      return (Nothing,Nothing)

--
-- Handlers for checking if the specified paper is supported.
--

getSupportedPaperR :: Handler RepJson
getSupportedPaperR = supportedPaperR lookupGetParam

postSupportedPaperR :: Handler RepJson
postSupportedPaperR = supportedPaperR lookupPostParam

supportedPaperR :: (T.Text -> GHandler App App (Maybe T.Text)) -> Handler RepJson
supportedPaperR f = do
  murl <- f "url"
  let
    mreader = murl >>= readerFromUrl
    pubname = maybe "N/A" (\r -> (readerName r) r) mreader
    pub = ["publisher" .= pubname]
    json = object (["supported" .= isJust mreader] ++ pub)
  jsonToRepJson json


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
  jsonToRepJson json

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
  jsonToRepJson json

jsonFormError :: Value
jsonFormError = object ["success" .= False, "message" .= ("Error while processing the form."::String)]

postRemovePapersR :: Handler RepJson
postRemovePapersR = formHandlerWithIdtext onetimeTokenF doRemove

postReparsePapersR :: Handler RepJson
postReparsePapersR = formHandlerWithIdtext onetimeTokenF doReparse

getReparsePaperR :: Handler RepJson
getReparsePaperR = do  -- caution: this is for a single pid, different from postReparsePapersR.
  mpid <- lookupGetParam "id"
  let mpidstr = mpid >>= fromPathPiece
  memail <- requireAuthId
  json <- case mpidstr of
    Just pid -> do
      doReparse [pid]
    Nothing -> do
      return $ object ["success" .= False, "message" .= ("No valid paper ID found."::String)] 
  jsonToRepJson json

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
  jsonToRepJson json

postAddTagsR = formHandlerWithIdtextAndParams onetimeTokenF doAddTags
postRemoveTagsR = formHandlerWithIdtextAndParams onetimeTokenF doRemoveTags

postReplaceTagsR :: Handler RepJson
postReplaceTagsR = formHandlerWithIdtextAndParams onetimeTokenF doReplaceTags
  

-- Actions
-- These are called from post***R functions.
-- These should not be directly called from a client to avoid CSRF.

doReparse :: [PaperId] -> Handler Value
doReparse pids = do
  email <- requireAuthId
  res <- runDB $ selectListUser email [PaperId <-. pids] []
  if null res then
    return $ object ["success" .= False
                     , "action" .= ("Reparse"::String)
                     , "message" .= ("Papers with these IDs were not found"::String)
                     , "id" .= map toPathPiece pids]
  else do
    json <- forM res $ \(Entity pid p) -> do
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
  email <- requireAuthId
  runDB $ deleteWhereUser email [PaperId <-. pids]
  let obj = map f pids
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
  -- let tags = filter (not . T.null) atags
  updatedtags <- forM ids $ \pid -> do
    mpaper <- runDB $ get pid
    case mpaper of
      Just paper -> do
        let newtags = L.union (paperTags paper) tags
        let newpaper = paper{paperTags=newtags}
        runDB $ replace pid newpaper
        return newtags
      Nothing ->
        return []
  let html = case (headMay ids,headMay updatedtags) of
            (Just pid,Just tags) -> paperTagsHtml pid tags
            (Nothing,Nothing) -> ""
  return $ object ["success" .= True,"replaceHtml" .= (renderHtml html)]
  
doRemoveTags :: [PaperId] -> [Text] -> Handler Value
doRemoveTags ids tags = do
  liftIO $ print (tags,ids)
  updatedtags <- forM ids $ \pid -> do
    paper <- runDB $ get404 pid
    let oldtags = paperTags paper
    let removed = filter (`elem` oldtags) tags
    let newtags = oldtags L.\\ removed
    let newpaper = paper{paperTags=newtags}
    runDB $ replace pid newpaper
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
  email <- requireAuthId
  idtext <- lookupPostParam "id"
  let ids = splitIdText idtext
  res <- runDB $ selectListUser email [PaperId <-. ids] []
  liftIO $ print ids
  let ps = map (\(Entity id p) -> p) res
  sendResponse $ RepPlain $ toContent $ T.intercalate "\n" (map PR.paperRIS ps)

postPaperRefsR = getPaperRefsR

getPaperRefsR :: PaperId -> Handler RepJson
getPaperRefsR pid = do
  email <- requireAuthId
  paper <- runDB $ get404 pid
  if paperUserEmail paper == Just email then do
    let refs = paperReferences paper
    jsonToRepJson $ Ae.object ["refs" .= ("NA"::String)] -- Stub!!! ["refs" .= (map Ae.toJSON refs)]
  else
    notFound


getPaperInfoAjaxR :: PaperId -> Handler RepHtml
getPaperInfoAjaxR pid = do
  paper <- runDB $ get404 pid
  nowrapLayout (paperInfoAjax paper)

getPaperCitationR :: PaperId -> Handler RepPlain
getPaperCitationR pid = do
  email <- requireAuthId
  paper <- runDB $ get404 pid
  if paperUserEmail paper == Just email then do
    res <- runDB $ selectListUser email [PaperId ==. pid] []
    let [Entity id p] = res
    sendResponse $ RepPlain $ toContent $ PR.paperRIS p
  else
    notFound

