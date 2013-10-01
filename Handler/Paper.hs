-- Handler.Paper


{-# LANGUAGE QuasiQuotes,DoAndIfThenElse,TemplateHaskell #-}

module Handler.Paper where

import Import

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL

import Data.Text.Lazy.Encoding as TL
import Data.Text.Encoding as T

import Safe

import Text.XML as X (Document)
import qualified Data.Aeson as Ae

import qualified Model.PaperReader as PR
import Text.Blaze.Html.Renderer.Text

import Handler.Widget
import Handler.Form
import Handler.Utils
import Handler.Render


import qualified Data.ByteString.Base64.URL as Base64 (decode)

import Model.Pubmed
import Model.RIS

import Model.PaperP
import Model.PaperMongo

import Control.Lens hiding ((.=))
import qualified Parser.Lens as L

import qualified Parser.Paper as P
import Parser.PaperReader (readerFromHtml,readerFromUrl,parseHtml)
import Parser.PaperReaderTypes

import Data.Time.Clock


import Control.Applicative
import Debug.Trace
import Network.HTTP.Conduit

import Data.String (fromString)


-- to use Html into forms
import Yesod.Form.Nic (YesodNic)
instance YesodNic App

--
-- Handlers for adding a paper.
--


--FIXME: All runDB functions should be changed to raw mongo.

getPaperJsonR :: PaperId -> Handler TypedContent
getPaperJsonR pid = do
  email <- requireAuthId'
  mp <- getPaperDB email pid
  case mp of
    Just p -> return $ toTypedContent (toJSON p)
    Nothing -> return $ toTypedContent $ object ["success" .= False, "message" .= ("Paper ID not found."::Text)]


postPaperJsonR = getPaperJsonR

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

postAddFromBookmarkletR :: Handler TypedContent
postAddFromBookmarkletR = do
  email <- requireAuthId'
  $(logInfo) $ T.append "postAddFromBookmarkletR from: " email
  [murl,mhtml] <- mapM lookupPostParam ["url","html"]
  -- $(logInfo) $ T.concat [fromMaybe "Nothing" url, " ", fromMaybe "Nothing" html]
  case (murl,mhtml) of
    (Just u,Just h) -> addIfNotExistBookmarklet email u h
    _ -> selectRep $ provideRep $ nowrapLayout $(widgetFile "bookmarklet_param_invalid")


addIfNotExistBookmarklet :: Text -> Text -> Text -> Handler TypedContent
addIfNotExistBookmarklet email url html = do
  res <- getPaperByUrl email url
  case res of
    Just (pid,paper) ->
      selectRep $ provideRep $ nowrapLayout $(widgetFile "bookmarklet_paper_exists")
    Nothing -> do
      mp <- htmlToPaper url html
      case mp of
        Just (pp,p) -> addIfNotExist email url html Nothing
        Nothing -> return $ failureJSON "Parsing cannot be done."
      -- selectRep $ provideRep $ nowrapLayout $(widgetFile "bookmarklet_paper_added")


-- |Entry point for adding a paper.
-- |POST params:
-- |url: Text, html: Text, serverside: Bool, parsed: stringified JSON
postAddPaperR :: Handler TypedContent
postAddPaperR = do
  email <- requireAuthId'
  $(logInfo) $ T.append "postAddPaperR from: " email
  [murl,mhtml,mserverside,mparsed] <- mapM lookupPostParam ["url","html","serverside","parsed"]
  case (murl,mhtml,mserverside,mparsed) of
    (Just u,Just h,Just "false",Just jsontxt) -> do
      mp <- jsonTxtToPaper jsontxt
      addIfNotExist email u h mp
    (Just u,Just h,_,_) -> do
      mp <- htmlToPaper u h     
      addIfNotExist email u h mp
    _ -> return $ toTypedContent $ object ["success" .= False,"message" .= ("Params are incomplete." :: Text)]


-- Add a re-parsed content from client.
postUpdatePaperR :: Handler TypedContent
postUpdatePaperR = do
  email <- requireAuthId'
  $(logInfo) $ T.append "postUpdatePaperR from: " email
  mparsed <- lookupPostParam "parsed"
  case mparsed of
    Just jsontxt -> updateWithParsed email jsontxt
    _ -> return $ toTypedContent $ object ["success" .= False,"message" .= ("Params are incomplete." :: Text)]

-- Ver.0.11 change:
-- Using DOI as identity. Different version with the same DOI is merged by availability flags.
addIfNotExist :: Text -> Text -> Text -> Maybe (P.Paper,Paper) -> Handler TypedContent
addIfNotExist _ _ _ Nothing = return $ toTypedContent $ object ["success" .= False, "message" .= ("Not valid data." :: Text)]
addIfNotExist email url html (Just (pp,p)) = do
  let doi = paperDoi p
  res <- getPaperByDOI email doi
  json <- case res of
    Just (present_id,present) -> do
      let ResourceAvailability cit abs full fig ref toc = paperAvailable present
      if not (cit && abs && full && fig && ref && toc) then
        doAdd email url html (Just (present_id, present)) pp p
      else
        return $ object ["success" .= False, "message" .= ("Already exists" :: Text),
                      "doi" .= (paperDoi present), "id" .= toPathPiece present_id,
                      "summary" .= object (paperSummary present_id present)]
    Nothing -> doAdd email url html Nothing pp p
  return $ toTypedContent json

htmlToPaper :: Url -> Text -> Handler (Maybe (P.Paper,Paper))
htmlToPaper url html = do
  mnewpp <- liftIO $ parseHtml url html
  let mnewp = fmap paperPToPaper mnewpp  -- Take out Paper from PaperP
  case mnewpp of
    Just newpp -> do
      time <- liftIO $ getCurrentTime
      return $ Just $ (newpp, (paperPToPaper newpp) {paperTimeAdded=time})
    Nothing -> return Nothing

jsonTxtToPaper :: Text -> Handler (Maybe (P.Paper,Paper))
jsonTxtToPaper jsontxt = do
  let mjson = Ae.decode (TL.encodeUtf8 $ TL.fromStrict jsontxt)
  let mnewpp = mjson >>= parseJsonFromClient
  case mnewpp of
    Just newpp -> do
      time <- liftIO $ getCurrentTime
      return $ Just $ (newpp, (paperPToPaper newpp) {paperTimeAdded=time})
    Nothing -> return Nothing

-- Unlike doAdd, this forces update.
updateWithParsed :: Text -> Text -> Handler TypedContent
updateWithParsed email jsontxt = do
  mps <- jsonTxtToPaper jsontxt
  case mps of
    Just (pp,p) -> do
      mp <- getPaperByUrl email (P._paperUrl pp)
      case mp of
        Just (pid,paper) -> do
          -- $(logInfo) $ "updatedWithParsed: success."
          let html = paperOriginalHtml paper
          updatePaperDB email pid $ (paperPToPaper pp){paperOriginalHtml=html,paperUserEmail=Just email}
          let json = object ["success" .= True, "message" .= ("Paper updated."::Text)]
          return $ toTypedContent $ json
        Nothing -> do
          return $ failureJSON "Paper with specified URL is not found."
    Nothing -> do
      return $ failureJSON "JSON fields missing or parse failure."

doAdd :: Text -> Text -> Text -> Maybe (PaperId,Paper) -> PaperP -> Paper -> Handler Value
doAdd email url html mpresent pp p = do
  mnewpp <- liftIO $ parseHtml url html
  let mnewp = fmap paperPToPaper mnewpp  -- Take out Paper from PaperP
  $(logInfo) $ "Paper got from parser"
  res <- case (mpresent,mnewp) of
    (Nothing,Just newp) -> do
      time <- liftIO $ getCurrentTime
      pid <- runDB $ insertUser email newp{paperTimeAdded=time}
      $(logInfo) "Paper added to DB"
      saveFormattedCache FormatB pid (fromJust mnewpp)   -- Stub: Also change reparse.
      time <- liftIO $ getCurrentTime
      let history = History (Just pid) HACreate time (User email Nothing Nothing) Nothing
      _ <- runDB $ insertUserH email history
      return (mnewp,Just pid)
    (Just (present_id,present),Just newp) -> do
      $(logInfo) "Merge paper to DB"
      paper <- liftIO $ mergeTwoPapers present newp
      updatePaperDB email present_id paper
      liftIO $ print present_id
      return (Just paper,Just present_id)
    (_,Nothing) -> do
      $(logWarn) $ "Parsing failure."
      return (Nothing,Nothing)
  return $ case res of
             (Just rep,Just pid) ->
              let
                cit = paperCitation rep
                journal = (citationJournal cit)
                useGScholar = not . all id $
                              [isJust journal,
                              isJust (citationTitle cit),
                              isJust (citationVolume cit),
                              isJust (citationYear cit),
                              isJust (citationPageFrom cit)
                              ]
                pubmedCovered j = True -- Stub.
                usePubmed = useGScholar && (maybe True pubmedCovered journal)
              in
                object [ "success" .= True, "summary" .= object (paperSummary pid rep),"usePubmed" .= usePubmed,
                          "useGScholar" .= useGScholar ]
             _ -> object [ "success" .= False , "message" .= ("No reader was found." :: String)]
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

-- Server side fetching.
getRefetchPubmedInfo :: Handler TypedContent
getRefetchPubmedInfo = do
  mpid <- lookupGetParam "id"
  mdoi <- lookupGetParam "doi"
  case (mpid >>= fromPathPiece, mdoi) of
    (Just pid,Just doi) -> do
      (minfo,txt) <- liftIO $ pubmedByDOI doi
      case minfo of
        Just info -> do
          doAddPubmedInfo pid (TL.toStrict txt) info
        _ -> return $ failureJSON "Pubmed fetching/parsing failure."
    _ ->
      return $ failureJSON "Params are missing."


postAddPubmedInfoR :: Handler TypedContent
postAddPubmedInfoR = do
  mpid' <- lookupPostParam "id"
  mxmlstr <- fmap decodeXml <$> lookupPostParam "xml"
  -- liftIO $ print xmlstr
  let mpid = mpid' >>= fromPathPiece
  let minfo = mxmlstr >>= pubmedFromXml
  case (mpid,minfo,mxmlstr) of
    (Just pid,Just info,Just xmlstr) ->
      doAddPubmedInfo pid xmlstr info
    (Just pid,Nothing,Just xmlstr) ->
      return $ failureJSON "XML parse error (Probably because there is no record on Pubmed)."
    _ -> return $ failureJSON "Parameters are missing."

doAddPubmedInfo :: PaperId -> Text -> PubmedInfo -> Handler TypedContent
doAddPubmedInfo pid txt info = do
  liftIO $ TIO.writeFile (fromString (appRootFolder++"data/pubmed/"++(T.unpack $ pmId info))) txt
  updatePaperWithPubmed info pid
  return $ toTypedContent $ object ["success" .= True, "pubmed_id" .= pmId info]

updatePaperWithPubmed :: PubmedInfo -> PaperId -> Handler ()
updatePaperWithPubmed info pid = do
  email <- requireAuthId'
  paper <- getPaperDB404 email pid
  let cit = paperCitation paper
  let authors = citationAuthors cit 
  let newcit = cit {citationJournal=citationJournal cit <|> pmJournal info,
                    citationTitle=citationTitle cit <|> pmTitle info,
                    citationVolume=citationVolume cit <|> pmVolume info,
                    citationYear=citationYear cit <|> pmYear info,
                    citationPageFrom=citationPageFrom cit <|> pmPageFrom info,
                    citationPageTo=citationPageTo cit <|> pmPageTo info,
                    citationAuthors= (if null authors then fromMaybe [] $ pmAuthors info else authors)}
  let newp = paper {paperCitation=newcit}
--  liftIO $ print info
--  liftIO $ print newcit
  updatePaperDB email pid newp
  return ()

-- Adding info from RIS format metainfo.
postAddGoogleScholarInfoR :: Handler TypedContent
postAddGoogleScholarInfoR = do
  email <- requireAuthId'
  mris <- lookupPostParam "ris"
  mpid <- lookupPostParam "id"
  case (mpid >>= fromPathPiece,mris) of
    (Just pid,Just ris) -> do
      case parseRIS ris of
        Just risinfo -> do
          updatePaperWithRIS risinfo pid
          return $ toTypedContent $ object ["success" .= True]
        _ -> return $ failureJSON "RIS file parse failure."
    _ -> return $ failureJSON "Parameters are missing."

updatePaperWithRIS :: RISInfo -> PaperId -> Handler ()
updatePaperWithRIS info pid = notFound -- stub: seems the following code has a weird bug that makes random change.
{-updatePaperWithRIS info pid = do
  email <- requireAuthId'
  paper <- getPaperDB404 email pid
  let cit = paperCitation paper
  let authors = citationAuthors cit 
  let newcit = cit {citationJournal=citationJournal cit <|> risJournal info,
                    citationTitle=citationTitle cit <|> risTitle info,
                    citationVolume=citationVolume cit <|> risVolume info,
                    citationYear=citationYear cit <|> risYear info,
                    citationPageFrom=citationPageFrom cit <|> risPageFrom info,
                    citationPageTo=citationPageTo cit <|> risPageTo info,
                    citationAuthors= (if null authors then fromMaybe [] $ risAuthors info else authors)}
  let newp = paper {paperCitation=newcit}
--  liftIO $ print info
--  liftIO $ print newcit
  updatePaperDB email pid newp
  return ()
-}

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

-- single paper removal.
postRemovePaperR :: PaperId -> Handler TypedContent
postRemovePaperR pid = do
  runDB $ delete pid
  return $ toTypedContent $ object ["success" .= True]

postRemovePapersR :: Handler TypedContent
postRemovePapersR = do
  email <- requireAuthId'
  idstxt <- lookupPostParam "id"
  fmap toTypedContent $ doRemove (splitIdText idstxt)

postReparsePapersR :: Handler TypedContent
postReparsePapersR = do
  email <- requireAuthId'
  idstxt <- lookupPostParam "id"
  fmap toTypedContent $ doReparse (splitIdText idstxt)
  
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
postReplaceTagsR = do
  email <- requireAuthId'
  mpidtxt <- lookupPostParam "id"
  let mpid = mpidtxt >>= fromPathPiece
  mtags <- lookupPostParam "tags"
  let tags = splitParamText mtags
  $(logInfo) $ T.pack $ show (mpid,mtags) 
  case (mpid,length tags > 0) of
    (Just pid,True) -> toTypedContent <$> doReplaceTags [pid] tags
    _ -> return $ failureJSON "You need id and tags params."

-- paperMisc field update. This is used with stringified JSON.
postReplaceMiscR :: Handler TypedContent
postReplaceMiscR = do
  email <- requireAuthId'
  mpidtxt <- lookupPostParam "id"
  let mpid = mpidtxt >>= fromPathPiece
  mjsontxt <- lookupPostParam "json"
  case (mpid,mjsontxt) of
    (Just pid,Just jsontxt) -> do
      runDB $ updateWhere [PaperId ==. pid, PaperUserEmail ==. Just email] [PaperMisc =. T.encodeUtf8 jsontxt]
      return $ toTypedContent $ object ["success" .= True, "message" .= ("Information changed." :: Text)]
    _ -> return $ failureJSON "Params are not valid."

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
                       "citation" .= (rep^.L.citation)
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
        updatePaperDB email pid newpaper
        return newtags
      Nothing ->
        return []
  let html = case (headMay ids,headMay updatedtags) of
            (Just pid,Just tags) -> paperTagsHtml pid tags
            (Nothing,Nothing) -> ""
  return $ object ["success" .= True,"message" .= ("Tags were replaced." :: Text)]
  
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
    updatePaperDB email pid newpaper
    return newtags
  let html = case (headMay ids,headMay updatedtags) of
            (Just pid,Just tags) -> paperTagsHtml pid tags
            (Nothing,Nothing) -> ""
  return $ object ["success" .= True]

doReplaceTags :: [PaperId] -> [Text] -> Handler Value
doReplaceTags ids tags = do
  email <- requireAuthId'
  runDB $ updateWhere [PaperId <-. ids, PaperUserEmail ==. Just email] [PaperTags =. tags]
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
  mp <- getPaperById email pid
  case mp of
    Just paper -> return $ toTypedContent $ Ae.object ["success" .= True, "refs" .= map toJSON (paperReferences paper)]
    Nothing -> return $ failureJSON "Paper ID not found."


getPaperInfoAjaxR :: PaperId -> Handler Html
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


failureJSON :: Text -> TypedContent
failureJSON msg = toTypedContent $ object ["success" .= False, "message" .= msg]

