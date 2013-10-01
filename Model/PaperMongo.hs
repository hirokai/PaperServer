-- Interface with MongoDB

module Model.PaperMongo where

import Import
import qualified Data.Text as T

import Data.String
import Data.Bson as Bson

import qualified Parser.Paper as P
import Database.MongoDB as DB
import Data.Bson -- ((!?))
import Database.Persist.MongoDB
import Data.Aeson as Ae

import qualified Data.ByteString as B (concat) 
import Data.ByteString.Lazy (fromChunks,toChunks)

import Data.Text.Encoding

import Model.PaperReader (mkCitHtml)

import Data.Time
import Control.Applicative

--
-- PaperSummary is used when you don't want large data such as full text.
-- 
data PaperSummary = PaperSummary {
  pTitle :: Text,
  pCit :: Citation,
  pId :: Text,
  pDoi :: Text,
  pTags :: [Text],
  pAvail :: ResourceAvailability,
  pAddedDate :: UTCTime,
  pMisc :: ByteString   -- Used for storing addition data (currently stringified JSON)
} deriving (Show,Eq)

--
-- High level functions to get/modify Paper(s)
--

getPaperById = getPaperDB

getPaperDB :: Text -> PaperId -> Handler (Maybe Paper)
getPaperDB email pid = do
  bson <- queryRawMongoOne $ paperById email pid
  -- liftIO $ print bson
  return $
    case bson of
      Just bs -> paperFromBson bs
      Nothing -> Nothing

getPapersByIds :: Text -> [PaperId] -> Handler [Paper]
getPapersByIds email ids = getPapersByFilter email ["_id" =: ["$in" =: map f ids]]
  where
    f pid = (read . T.unpack . toPathPiece) pid :: ObjectId

getRawHtmlById :: Text -> PaperId -> Handler (Maybe Text)
getRawHtmlById email pid = queryRawMongoOne $ rawHtmlById email pid

getPaperByUrl :: Text -> Url -> Handler (Maybe (PaperId,Paper))
getPaperByUrl email url = do
  mbson <- queryRawMongoOne $ paperByFilter email ["url" =: url]
  let res = case mbson of
              Just bs ->
                case ((bs !? "_id" :: Maybe ObjectId) >>= (fromPathPiece . T.pack . show),paperFromBson bs) of
                  (Just i,Just p) -> Just (i,p)
              Nothing -> Nothing
  return res

getPaperByDOI :: Text -> Text -> Handler (Maybe (PaperId,Paper))
getPaperByDOI email doi = do
  mbson <- queryRawMongoOne $ paperByFilter email ["doi" =: doi]
  let res = case mbson of
              Just bs ->
                case ((bs !? "_id" :: Maybe ObjectId) >>= (fromPathPiece . T.pack . show),paperFromBson bs) of
                  (Just i,Just p) -> Just (i,p)
              Nothing -> Nothing
  return res

getPaperMisc :: Text -> PaperId -> Handler (Maybe Ae.Value)
getPaperMisc email pid = do
  let on = 1 :: Int
  mbson <- queryRawMongoOne $ findOne $ (DB.select ["user_email" =: email, "_id" =: fromPid pid] "paper"){DB.project = ["misc" =: on]}
  let mmisc = mbson >>= (!? "misc")
  return $ case mmisc of
    Just (Binary bin) -> (Ae.decode . fromStrict) bin
    _ -> Nothing

updatePaperMisc :: Text -> PaperId -> Ae.Value -> Handler Bool
updatePaperMisc email pid val = do
  liftIO $ print "updatePaperMisc"
  liftIO $ print val
  queryRawMongoUnit $ DB.modify (DB.select ["_id" =: fromPid pid, "user_email" =: email]  "paper")
                                ["$set" =: ["misc" =: Binary (toStrict $ Ae.encode val)]]
  return True

toStrict = B.concat . toChunks
fromStrict = fromChunks . (:[])

--For performance reason, there are two functions, getPaperByFilter and getPapersByFilter
getPaperByFilter :: Text -> Bson.Document -> Handler (Maybe Paper)
getPaperByFilter email filt = do
  bson <- queryRawMongoOne $ paperByFilter email filt
  return $
    case bson of
      Just bs -> paperFromBson bs
      Nothing -> Nothing

getPapersByFilter :: Text -> Bson.Document -> Handler [Paper]
getPapersByFilter email filt = queryRawMongo $ papersByFilter email filt

getSummariesByFilter :: Text -> Bson.Document -> Handler [PaperSummary]
getSummariesByFilter email filt = queryRawMongo $ summariesFilter email filt

-- This function assumes query has correct user_email for filtering.
getSummariesByQuery :: DB.Query -> Handler [PaperSummary]
getSummariesByQuery query = do
  sums <- queryRawMongo $ do
    c <- DB.find query
    res <- DB.rest c
    return $ map fromBson res
  return $ catMaybes sums

countPapersByFilter :: Text -> Bson.Document -> Handler Int
countPapersByFilter email filt = do
  c <- queryRawMongoOne $ countMatching email filt
  return $ fromMaybe 0 c

-- This function assumes query has correct user_email for filtering.
countPapersByQuery :: DB.Query -> Handler Int
countPapersByQuery query = do
  c <- queryRawMongoOne $ fmap Just $ DB.count query
  return $ fromMaybe 0 c

-- FIXME: Use raw mongo. To do that, Make Val instance of Paper. 
updatePaperDB :: Text -> PaperId -> Paper -> Handler Bool
updatePaperDB email pid p = do
  runDB $ Database.Persist.MongoDB.replace pid p
  return True
  -- queryRawMongoUnit $ DB.save "paper" (bsonFromPaper pid p)


getPaperDB404 :: Text -> PaperId -> Handler Paper
getPaperDB404 email pid = do
  mp <- getPaperDB email pid
  case mp of
    Just p -> return p
    Nothing -> notFound


-- allOfUser :: (MonadBaseControl IO m, Monad m,Control.Monad.IO.Class.MonadIO m) => Text -> Action m [Bson.Document]
-- allSummariesOfUser :: Text -> m a [Document]
summaryList = ["url" =: on,"citation" =: on,"doi" =: on,"tags" =: on,
                             "available" =: on,"time_added" =: on,"misc" =: on]
  where on = 1 :: Int



-- allOfUser email = summariesFilter email id

--
-- Execution functions for DB.Action's.
--

-- FIXME: Use connection pool of Yesod to avoid connecting every time.
queryRawMongoOne :: DB.Action Handler (Maybe a) -> Handler (Maybe a)
queryRawMongoOne action = do
  app <- getYesod
  let dbconf = persistConfig app
  let (Just (MongoAuth user pass)) = mgAuth dbconf
  pipe <- liftIO $ DB.runIOE $ DB.connect (DB.host $ T.unpack (mgHost dbconf))
  eith <- DB.access pipe DB.ReadStaleOk "PaperServer" (doAuth user pass >> action)
  liftIO $ DB.close pipe
  return $ case eith of
              Left err -> Nothing
              Right p -> p

-- FIXME: Use connection pool of Yesod to avoid connecting every time.
queryRawMongo :: DB.Action Handler [a] -> Handler [a]
queryRawMongo action = do
  app <- getYesod
  let dbconf = persistConfig app
  let (Just (MongoAuth user pass)) = mgAuth dbconf
  pipe <- liftIO $ DB.runIOE $ DB.connect (DB.host $ T.unpack (mgHost dbconf))
  eith <- DB.access pipe DB.ReadStaleOk "PaperServer" (doAuth user pass >> action)
  liftIO $ DB.close pipe
  let recs = case eith of
              Left err -> []
              Right ps -> ps
  return recs

queryRawMongoUnit :: DB.Action Handler () -> Handler Bool
queryRawMongoUnit action = do
  app <- getYesod
  let dbconf = persistConfig app
  let (Just (MongoAuth user pass)) = mgAuth dbconf
  pipe <- liftIO $ DB.runIOE $ DB.connect (DB.host $ T.unpack (mgHost dbconf))
  eith <- DB.access pipe DB.ReadStaleOk "PaperServer" (doAuth user pass >> action)
  liftIO $ DB.close pipe
  return $ case eith of
              Left err -> False
              Right p -> True

--
-- Actions for Database.MongoDB.Query
-- These are used combined with queryRawMongoOne and queryRawMongo
--
{-
countMatching :: Text -> Bson.Document -> DB.Action Handler Int
countMatching email filt = do
  let q = (DB.select (["user_email" =: email] ++ filt) "paper") :: DB.Query
  app <- getYesod
  let dbconf = persistConfig app
  pipe <- liftIO $ DB.runIOE $ DB.connect (DB.host $ T.unpack (mgHost dbconf))
  eith <- DB.access pipe DB.ReadStaleOk "PaperServer" (DB.count q)
  liftIO $ DB.close pipe
  let len = case eith of
              Left err -> 0
              Right l -> l
  return len
-}

doAuth :: (MonadIO m, Applicative m) => Text -> Text -> DB.Action m Bool
doAuth u p = DB.auth u p

countMatching :: Text -> Bson.Document -> DB.Action Handler (Maybe Int)
countMatching email filt = do
  c <- DB.count (DB.select (["user_email" =: email]++filt) "paper")
  return $ Just c

collectAllTags :: Text -> DB.Action Handler [Text]
collectAllTags email = do
  let q = (DB.select ["user_email" =: email] "paper")
  vs <- DB.distinct "tags" q
  return $ map (\(DB.String t) -> t) vs


collectTags :: Text -> Document -> DB.Action Handler [[Text]]
collectTags email filt = do
  let on = 1 :: Int
  let q = (DB.select (filt ++ ["user_email" =: email]) "paper")
  liftIO $ print q
  cur <- DB.find q{DB.project = ["tags" =: on]}
  ts <- DB.rest cur
  return $ map convert ts
  where
    convert :: DB.Document -> [Text]
    convert doc = fromMaybe [] $ doc !? "tags"


summariesFilter :: Text -> Document -> DB.Action Handler [PaperSummary]
summariesFilter email filt = do
    let q = DB.select (["user_email" =: email] ++ filt) "paper"
    let qq = q{DB.project = summaryList}
--    liftIO $ print qq
    cur <- DB.find qq
    res <- DB.rest cur
    return $ catMaybes $ map fromBson res

fromPid :: PaperId -> Bson.ObjectId
fromPid = read . T.unpack . toPathPiece

paperById :: Text -> PaperId -> DB.Action Handler (Maybe Bson.Document)
paperById email pid = do
  paperByFilter email
    ["_id" =: fromPid pid]

rawHtmlById :: Text -> PaperId -> DB.Action Handler (Maybe Text)
rawHtmlById email pid = do
    let q = DB.select (["user_email" =: email, "_id" =: fromPid pid]) "paper"
    let qq = q{DB.project = ["original_html" =: (1::Int)]}
    md <- DB.findOne qq
    return $ md >>= Bson.lookup "original_html"

paperByFilter :: Text -> Bson.Document -> DB.Action Handler (Maybe Bson.Document)
paperByFilter email filt = do
    let q = DB.select (["user_email" =: email] ++ filt) "paper"
    liftIO $ print q
    DB.findOne q

-- papersByFilter :: (Functor m, MonadIO m) => Text -> Bson.Document -> Action m [Bson.Document]
papersByFilter :: Text -> Bson.Document -> DB.Action Handler [Paper]
papersByFilter email filt = do
    let q = DB.select (["user_email" =: email] ++ filt) "paper"
    liftIO $ print q
    c <- DB.find q
    res <- DB.rest c
    return $ catMaybes $ map paperFromBson res

-- This is only used for Handler.PaperListW2UI. Should be removed at some point.
defaultQuery :: Text -> DB.Query
defaultQuery email = select ["user_email" =: email] "paper"

--
-- BSON <=> Paper data types conversion.
--

bsonFromPaper :: PaperId -> Paper -> Bson.Document
bsonFromPaper
  pid
  (Paper doi url orig abs mainh mcit refs figs res toc tags note misc pinfo supp mavail email time)
  = undefined

paperFromBson :: Bson.Document -> Maybe Paper
paperFromBson doc =
  let
    fromObjId :: Bson.ObjectId -> Text
    fromObjId oid = T.pack $ show oid
    mcit = doc !? "citation"
    mavail = do
      txt <- doc !? "available"
      return $ f (T.splitOn "," txt)
        where
          f ts = ResourceAvailability
                   ("cit" `elem` ts) 
                   ("abs" `elem` ts) 
                   ("full" `elem` ts) 
                   ("fig" `elem` ts) 
                   ("ref" `elem` ts) 
                   ("toc" `elem` ts)
    doi = doc !? "doi"
    url = doc !? "url"
    orig = doc !? "original_html"
    abs = doc !? "abstract"
    mainh = doc !? "main_html"
    refs = fromMaybe [] $ doc !? "references"
    figs = fromMaybe [] $ doc !? "figures"
    res = fromMaybe [] $ doc !? "resources"
    toc = doc !? "toc"
    tags = fromMaybe [] $ doc !? "tags"
    note = doc !? "note"
    misc = (fromString . T.unpack) $ fromMaybe "" $ doc !? "misc" :: ByteString
    pinfo = doc !? "parser_info"
    supp = doc !? "support_level"
    email = doc !? "user_email"
    time = doc !? "time_added"
  in
  Paper
    <$> doi
    <*> url
    <*> orig
    <*> Just abs
    <*> Just mainh
    <*> mcit
    <*> Just refs
    <*> Just figs
    <*> Just res
    <*> Just toc
    <*> Just tags
    <*> Just note
    <*> Just misc
    <*> Just pinfo
    <*> supp
    <*> mavail
    <*> email
    <*> time

instance Val Figure where
  val (Figure fid name annot img local) =
    Doc ["figId" =: fid
        ,"name" =: name
        ,"annot" =: annot
        ,"img" =: img
        ,"localCopy" =: local]
  cast' (Doc doc) =
    Figure
      <$> doc !? "figId"
      <*> doc !? "name"
      <*> doc !? "annot"
      <*> doc !? "img"
      <*> doc !? "localCopy"
  cast' _ = Nothing

instance Val Reference where
  cast' (Doc doc) =
    Reference
      <$> (doc !? "refId")
      <*> (doc !? "refName")
      <*> Just (doc !? "cit")
      <*> Just (doc !? "citText")
      <*> Just (doc !? "url")
  cast' _ = Nothing
  val (Reference rid name cit txt url) =
    Doc [
      "refId" =: rid
     , "refName" =: name
     , "cit" =: cit
     , "citText" =: txt
     , "url" =: url
    ]

instance Val Resource where
  cast' (Doc doc) =
    Resource
      <$> (doc !? "resId")
      <*> (doc !? "url")
      <*> (doc !? "type")
      <*> (doc !? "path")
  cast' _ = Nothing
  val (Resource rid url typ path) =
    Doc [
      "resId" =: rid
      ,"url" =: url
      ,"type" =: typ
      ,"path" =: path
      ]

instance Val P.SupportLevel where
  cast' (Bson.String "fulltext") = Just P.SFullText
  cast' (Bson.String "abstract") = Just P.SAbstract
  cast' (Bson.String "citation") = Just P.SCitation
  cast' (Bson.String "unknown") = Just P.SUndecidable
  cast' _ = Nothing
  val P.SFullText = Bson.String "fulltext" 
  val P.SAbstract = Bson.String "abstract" 
  val P.SCitation = Bson.String "citation" 
  val P.SUndecidable = Bson.String "Unknown" 


instance Val Citation where
  cast' (Doc doc) =
    let
      d = doc !? "doi"
      u = doc !? "url"
      t = doc !? "title"
      j = doc !? "journal"
      y = doc !? "year"
      v = doc !? "volume"
      pf = doc !? "pageFrom"
      pt = doc !? "pageTo"
      as = fromMaybe [] $ doc !? "authors"
      pub = doc !? "publisher"
      typ = doc !? "type"
    in
      Just $ Citation d u t j y v pf pt as pub typ
  cast' _ = Nothing
  val (Citation d u t j y v pf pt as pub typ)
    = Doc
      ["doi" =: d
      , "url" =: u
      , "title" =: t
      , "journal" =: j
      , "year" =: y
      , "volume" =: v
      , "page_from" =: pf
      , "page_to" =: pt
      , "authors" =: as
      , "publisher" =: pub
      , "type" =: typ
      ] 

instance Val LocalCopyStatus where
  val LocalAvailable = Int32 $ fromIntegral 0
  val NotYet = Int32 $ fromIntegral 1
  val Failed = Int32 $ fromIntegral 2
  val Unknown = Int32 $ fromIntegral 3
  cast' (Int32 0) = Just LocalAvailable
  cast' (Int32 1) = Just NotYet
  cast' (Int32 2) = Just Failed
  cast' _ = Just Unknown




--
-- PaperSummary conversion functions
--

instance ToJSON PaperSummary where
  toJSON (PaperSummary title cit pid doi tags avail date misc)
    = object [
        "title" .= title
        , "citation" .= cit
        , "id" .= pid
        , "doi" .= doi
        , "tags" .= tags
        , "availability" .= avail
        , "addedDate" .= date
        , "misc" .= decodeUtf8 misc
        ]
{-
mkPSVals (PaperSummary t c i d tags avail date) =
  let
    --Stub!!
  in
  ["citation" .= c
   , "cittext" .= mkCitHtml c 
   , "available" .= avail
   , "tags" .= tags
   , "id" .= i
   , "date" .= date]  -- Stub!!
-}

fromBson :: Bson.Document -> Maybe PaperSummary
fromBson doc =
  let
    fromObjId :: Bson.ObjectId -> Text
    fromObjId oid = T.pack $ show oid
    mcit =
      let
        d = doc !? "citation.doi"
        u = doc !? "citation.url"
        t = doc !? "citation.title"
        j = doc !? "citation.journal"
        y = doc !? "citation.year"
        v = doc !? "citation.volume"
        pf = doc !? "citation.pageFrom"
        pt = doc !? "citation.pageTo"
        as = fromMaybe [] $ doc !? "citation.authors"
        pub = doc !? "citation.publisher"
        typ = doc !? "citation.type"
      in
        Just $ Citation d u t j y v pf pt as pub typ 
    mavail = do
      txt <- doc !? "available"
      return $ f (T.splitOn "," txt)
        where
          f ts = ResourceAvailability
                   ("cit" `elem` ts) 
                   ("abs" `elem` ts) 
                   ("full" `elem` ts) 
                   ("fig" `elem` ts) 
                   ("ref" `elem` ts) 
                   ("toc" `elem` ts)
    mdate = doc !? "time_added"
    mmisc = case doc !? "misc" of
              Just (Binary m) -> Just m
              Nothing -> Nothing
  in
  PaperSummary
    <$> doc !? "citation.title"
    <*> mcit
    <*> fmap fromObjId (doc !? "_id")
    <*> doc !? "doi"
    <*> doc !? "tags"
    <*> mavail
    <*> mdate
    <*> mmisc