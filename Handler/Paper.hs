-- Handler.Paper


{-# LANGUAGE QuasiQuotes,DoAndIfThenElse,TemplateHaskell #-}

module Handler.Paper where

import Import

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL

import Data.Text.Lazy.Encoding

import Safe

import qualified Data.Aeson as Ae
import Data.Aeson.Types (parseMaybe)
import qualified Model.PaperReader as PR
import Text.Blaze.Html.Renderer.Text

import Handler.Widget
import Handler.Form
import Handler.Utils
import Handler.Render

import Model.PaperP
import Model.PaperMongo

import Control.Lens hiding ((.=))

import qualified Parser.Paper as P
import Parser.PaperReader (readerFromHtml,readerFromUrl,parseHtml)
import Parser.PaperReaderTypes

import Data.Time.Clock

import Text.XML as X
import Text.XML.Cursor as C
import Text.XML.Selector
import Text.XML.Selector.TH
import Text.XML.Scraping

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

-- This returns HTML
-- ToDo: Currently this uses URL for identity, but there may be a better choice.
-- DOI is not good, because the abstract and fulltext (SAbstract and SFullText) have the same DOI.
addIfNotExistBookmarklet :: Text -> Text -> Text -> Handler TypedContent
addIfNotExistBookmarklet email url html = do
  res <- getPaperByUrl email url
  case res of
    Just (pid,paper) ->
      selectRep $ provideRep $ nowrapLayout $(widgetFile "bookmarklet_paper_exists")
    Nothing -> do
      json <- doAdd email url html
      return $ toTypedContent json
      -- selectRep $ provideRep $ nowrapLayout $(widgetFile "bookmarklet_paper_added")


{-}
optionsAddFromBookmarkletR :: Handler RepPlain
optionsAddFromBookmarkletR = do
    -- ToDo: Add other sites.
    setHeader "Access-Control-Allow-Headers" "Origin, Access-Control-Allow-Origin, X-Requested-With, Content-Type, Accept"
    setHeader "Access-Control-Allow-Origin" "*"
    setHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"
    return $ RepPlain $ toContent ("" :: Text)
 -}


-- |Entry point for adding a paper.
-- |This uses two POST params, url and html.
postAddPaperR :: Handler TypedContent
postAddPaperR = do
  email <- requireAuthId'
  $(logInfo) $ T.append "postAddPaperR from: " email
  [murl,mhtml,mserverside,mparsed] <- mapM lookupPostParam ["url","html","serverside","parsed"]
  -- $(logInfo) $ T.concat [fromMaybe "Nothing" url, " ", fromMaybe "Nothing" html]
--  liftIO $ print (murl,isJust mhtml,mserverside,mparsed)
  case (murl,mhtml,mserverside,mparsed) of
    (Just u,Just h,Just "false",Just jsontxt) -> addIfNotExistWithParsed email u h jsontxt
    (Just u,Just h,_,_) -> addIfNotExist email u h
    _ -> return $ toTypedContent $ object ["success" .= False,"message" .= ("Params are incomplete." :: Text)]


addIfNotExistWithParsed :: Text -> Text -> Text -> Text -> Handler TypedContent
addIfNotExistWithParsed email url html jsontxt = do
  res <- getPaperByUrl email url
  json <- case res of
    Just (pid,paper) ->
      return $ object ["success" .= False, "message" .= ("Already exists" :: Text),
                      "doi" .= (paperDoi paper), "id" .= toPathPiece pid, "summary" .= object (paperSummary pid paper)]
    Nothing -> doAddParsed email url html jsontxt
  return $ toTypedContent json

-- ToDo: Currently this uses URL for identity, but there may be a better choice.
-- DOI is not good, because the abstract and fulltext (SAbstract and SFullText) have the same DOI.
addIfNotExist :: Text -> Text -> Text -> Handler TypedContent
addIfNotExist email url html = do
  res <- getPaperByUrl email url
  json <- case res of
    Just (pid,paper) ->
      return $ object ["success" .= False, "message" .= ("Already exists" :: Text),
                      "doi" .= (paperDoi paper), "id" .= toPathPiece pid, "summary" .= object (paperSummary pid paper)]
    Nothing -> doAdd email url html
  return $ toTypedContent json

doAddParsed :: Text -> Text -> Text -> Text -> Handler Value
doAddParsed email url html jsontxt = do
  let mjson = Ae.decode (encodeUtf8 $ TL.fromStrict jsontxt)
  liftIO $ print mjson
  let mnewpp = mjson >>= parseJsonFromClient
  case mnewpp of
    Just newpp -> do
      $(logInfo) $ "Received parsed JSON from client."
      time <- liftIO $ getCurrentTime
      let newp = paperPToPaper newpp
      pid <- runDB $ insertUser email newp {paperTimeAdded=time}
      $(logInfo) "Paper added to DB"
      saveFormattedCache FormatB pid newpp   -- Stub: Also change reparse.
      time <- liftIO $ getCurrentTime
      let history = History (Just pid) HACreate time (User email Nothing Nothing) Nothing
      _ <- runDB $ insertUserH email history
      return $ object ["success" .= True, "summary" .= object (paperSummary pid newp)]
    _ -> return $ object ["success" .= False, "message" .= ("JSON from client: parse error." :: Text), "url" .= url]

-- Stub
parseJsonFromClient :: Value -> Maybe P.Paper
parseJsonFromClient (Object v) =
  flip parseMaybe v $ \v -> do
    doi <- v .: "doi"
    url <- v .: "url"
    abs <- v .: "abstract"
    cit <- v .: "citation"
    return $ P.emptyPaper{P._paperDoi=doi,P._paperUrl=url,P._paperAbstract=abs,
                            P._paperCitation=(addCitMaybe cit)  -- Maybe is not parsed.:
                            -- Pitfall: http://hackage.haskell.org/packages/archive/aeson/0.6.1.0/doc/html/Data-Aeson.html
                          }

    where
      addCitMaybe :: CitationNoMaybe -> P.Citation
      addCitMaybe (CitationNoMaybe a b c d e f g h i j k) =
        P.Citation (Just a) (Just b) (Just c) (Just d) (Just e) (Just f) (Just g) (Just h) i (Just j) (Just k)

data CitationNoMaybe = CitationNoMaybe Text Text Text Text Int Text Text Text [Text] Text Text

instance FromJSON CitationNoMaybe where
  parseJSON (Object v) =
    CitationNoMaybe
      <$> v .: "doi"
      <*> v .: "url"
      <*> v .: "title"
      <*> v .: "journal"
      <*> v .: "year"
      <*> v .: "volume"
      <*> v .: "pageFrom"
      <*> v .: "pageTo"
      <*> v .: "authors"
      <*> v .: "publisher"
      <*> v .: "type"

  parseJSON _ = mzero

 {- P.Paper
    <$> v .: "doi"
    <*> v .: "url"
    <*> v .: "html"
    <*> v .: "abstract"
    <*> v .: "mainHtml"
    <*> v .: "citation"
    <*> v .: "references"
    <*> v .: "figures"
    <*> v .: "resources"
    <*> v .: "toc"
    <*> v .: "misc"
    <*> v .: "sections" -}

{-

data Paper = Paper {
  _paperDoi :: Text,
  _paperUrl :: Text,
  _paperHtml :: Text,
  _paperAbstract :: Maybe Text,
  _paperMainHtml :: Maybe PaperMainText,
  _paperCitation ::Citation,
  _paperReferences :: [Reference],
  _paperFigures ::[Figure],
  _paperResources :: [Resource],
  _paperToc :: Maybe Text,
  _paperTags :: [Text],
  _paperNote :: Maybe Text,
  _paperMisc :: ByteString,     -- Not used now.
  _paperSections :: Maybe SectionInfo,
  _paperParserInfo :: Maybe Text,
  -- _paperUserEmail :: Maybe Text
  _paperSupportLevel :: SupportLevel
} deriving (Show)

-}
doAdd :: Text -> Text -> Text -> Handler Value
doAdd email url html = do
  let mreader = readerFromHtml url html
  -- liftIO $ TIO.writeFile (CachePath email url) html
  -- runDB $ deleteWhereUser email [PaperUrl ==. url] -- In case paper exists but no html is found.
  liftIO $ print (url,isJust mreader)
  mrep <- case mreader of
                Just r -> do
                  $(logInfo) $ T.append "Reader selected: " ((\a -> (readerName a) a) r)
                  parseAndAdd email r url html
                Nothing -> do
                  $(logWarn) "No Reader was found."
                  return (Nothing,Nothing)
  return $ case mrep of
             (Just rep,Just pid) ->
              let
                cit = paperCitation rep
                usePubmed = not . all id $
                              [isJust (citationJournal cit),
                              isJust (citationTitle cit),
                              isJust (citationVolume cit),
                              isJust (citationYear cit),
                              isJust (citationPageFrom cit)
                              ]
              in
                object [ "success" .= True, "summary" .= object (paperSummary pid rep),"usePubmed" .= usePubmed]
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

-- Server side fetching.
getRefetchPubmedInfo :: Handler TypedContent
getRefetchPubmedInfo = do
  mpid <- lookupGetParam "id"
  mdoi <- lookupGetParam "doi"
  case (mpid >>= fromPathPiece, mdoi) of
    (Just pid,Just doi) -> do
      let url = T.concat ["http://www.ncbi.nlm.nih.gov/pubmed?term=",doi,"&report=xml&format=text"]
      liftIO $ print (pid,doi,url)
      txt <- decodeUtf8 <$> simpleHttp (T.unpack url)
      let edoc = parseText def txt
      json <- case edoc of
                    Right doc ->
                      doAddPubmedInfo pid (TL.toStrict txt) (adhocFixDoc doc)
                    _ -> return $ object ["success" .= False]
      return $ toTypedContent json
    _ ->
      return $ toTypedContent $ object ["success" .= False, "message" .= ("Params are missing."::Text)]

adhocFixDoc :: Document -> Document
adhocFixDoc doc =
  maybe doc mkDoc $ L.find isContent $ elementNodes $ documentRoot doc
  where
    mkDoc (NodeContent txt) =
      case parseText def $ TL.fromStrict txt of
        Right d -> d
        _ -> doc
    isContent (NodeContent _) = True
    isContent _ = False

postAddPubmedInfoR :: Handler TypedContent
postAddPubmedInfoR = do
  mpid' <- lookupPostParam "id"
  mxmlstr <- fmap decodeXml <$> lookupPostParam "xml"
  -- liftIO $ print xmlstr
  let mpid = mpid' >>= fromPathPiece
  let mdoc = case mxmlstr of
                Just xmlstr ->
                  case parseText def (TL.fromStrict xmlstr) of
                         Right d -> Just d
                         _ -> Nothing
                _ -> Nothing
  json <- case (mpid,mdoc) of
            (Just pid,Just doc) ->
              doAddPubmedInfo pid (fromMaybe "" mxmlstr) doc
            _ -> return $ object ["success" .= False,"message" .= ("XML parse error."::Text)]
  return $ toTypedContent json

doAddPubmedInfo :: PaperId -> Text -> Document -> Handler Value
doAddPubmedInfo pid txt doc = do
 -- liftIO $ print doc
  info <- parsePubmedXml doc
  liftIO $ X.writeFile def (fromString (appRootFolder++"data/pubmed/"++(T.unpack $ pmId info))) doc -- not using txt
  updatePaperWithPubmed info pid
  return $ object ["success" .= True]

decodeXml :: Text -> Text
decodeXml txt = T.replace "&lt;" "<" . T.replace "&gt;" ">" $ txt

data PubmedInfo = PubmedInfo {
  pmId :: Text,
  pmTitle :: Maybe Text,
  pmJournal :: Maybe Text,
  pmAuthors :: Maybe [Text],
  pmVolume :: Maybe Text,
  pmYear :: Maybe Int,
  pmPageFrom :: Maybe Text,
  pmPageTo :: Maybe Text
} deriving (Eq,Show)

parsePubmedXml :: Document -> Handler PubmedInfo
parsePubmedXml doc = do
  let
    c = fromDocument doc
    title = maybeText $ TL.toStrict $ innerText $ (c $// C.element "ArticleTitle")
    pubmedId = fromMaybe "NA" $ do
                  h <- headMay $ (c $// C.element "MedlineCitation" &/ C.element "PMID")
                  return $ TL.toStrict $ innerText h
    journal = do
      h <- headMay (c $// C.element "MedlineCitation" &// C.element "Title")
      maybeText $ TL.toStrict $ innerText h
    authors = map mkName $ (c $// C.element "Author")
    pages = mkPages $ TL.toStrict $ innerText $ (c $// C.element "MedlinePgn")
    mkPages :: Text -> (Maybe Text,Maybe Text)
    mkPages txt =
      let
        ts = T.splitOn "-" txt
        pf = atMay ts 0
        pt = atMay ts 1
        f from to = (T.take (T.length from - T.length to) from) `T.append` to
      in
        (pf, liftA2 f pf pt)
    mkName :: Cursor -> Text
    mkName cur =
      let
        ln = TL.toStrict $ innerText $ (cur $.// C.element "LastName")
        fn = TL.toStrict $ innerText $ (cur $.// C.element "ForeName")
      in
        T.concat [fn," ",ln]
    volume = maybeText $ TL.toStrict $ innerText $ (c $// C.element "Volume")
    year = do
      c <- headMay $ (c $// C.element "PubDate" &// C.element "Year")
      readMay $ TL.unpack $ innerText $ c
--  liftIO $ print (title,journal,doc) 
  return $ PubmedInfo pubmedId title journal (if null authors then Nothing else Just authors) volume year (fst pages) (snd pages)

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
  updatePaperDB pid newp
  return ()

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

