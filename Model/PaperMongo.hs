-- Interface with MongoDB

module Model.PaperMongo where

import Import
import qualified Data.Text as T
-- import Safe

-- import Control.Applicative

import Data.String

-- import Data.Bson ((!?),(=:))
import Data.Bson as Bson

import qualified Parser.Paper as P
-- import Database.MongoDB ((=:))
import Database.MongoDB as DB
-- import qualified Database.MongoDB.Query as DB
import Data.Bson -- ((!?))
-- import qualified Data.Bson as Bson
import Database.Persist.MongoDB

updatePaperDB :: PaperId -> Paper -> Handler ()
updatePaperDB pid p = runDB $ Database.Persist.MongoDB.replace pid p   -- FIXME: Use raw mongo. Make Val instance of Paper

getPaperDB404 :: Text -> PaperId -> Handler Paper
getPaperDB404 email pid = do
  mp <- getPaperDB email pid
  case mp of
    Just p -> return p
    Nothing -> notFound

paperById :: MonadIO m => Text -> PaperId -> DB.Action m (Maybe Bson.Document)
paperById email pid =
  paperByFilter email
    ["_id" =: (read $ T.unpack $ toPathPiece pid :: Bson.ObjectId)]


paperByFilter :: MonadIO m => Text -> Bson.Document -> DB.Action m (Maybe Bson.Document)
paperByFilter email filt = do
    let q = DB.select (["user_email" =: email] ++ filt) "paper"
    liftIO $ print q
    DB.findOne q

-- papersByFilter :: (Functor m, MonadIO m) => Text -> Bson.Document -> Action m [Bson.Document]
papersByFilter email filt = do
    let q = DB.select (["user_email" =: email] ++ filt) "paper"
    liftIO $ print q
    c <- DB.find q
    DB.rest c

queryRawMongoOne action = do
  app <- getYesod
  let dbconf = persistConfig app
  pipe <- liftIO $ DB.runIOE $ DB.connect (DB.host $ T.unpack (mgHost dbconf))
  eith <- DB.access pipe DB.ReadStaleOk "PaperServer" action
  liftIO $ DB.close pipe
  return $ case eith of
              Left err -> Nothing
              Right p -> p

queryRawMongo action = do
  app <- getYesod
  let dbconf = persistConfig app
  pipe <- liftIO $ DB.runIOE $ DB.connect (DB.host $ T.unpack (mgHost dbconf))
  eith <- DB.access pipe DB.ReadStaleOk "PaperServer" action
  liftIO $ DB.close pipe
  let recs = case eith of
              Left err -> []
              Right ps -> ps
  return recs


getPaperById = getPaperDB

getPaperDB :: Text -> PaperId -> Handler (Maybe Paper)
getPaperDB email pid = do
  bson <- queryRawMongoOne $ paperById email pid
  -- liftIO $ print bson
  return $
    case bson of
      Just bs -> paperFromBson bs
      Nothing -> Nothing

getPaperByFilter :: Text -> Bson.Document -> Handler (Maybe Paper)
getPaperByFilter email filt = do
  bson <- queryRawMongoOne $ paperByFilter email filt
  return $
    case bson of
      Just bs -> paperFromBson bs
      Nothing -> Nothing

getPapersByIds email ids = getPapersByFilter email ["_id" =: ["$in" =: map f ids]]
  where
    f pid = (read . T.unpack . toPathPiece) pid :: ObjectId

getPapersByFilter :: Text -> Bson.Document -> Handler [Paper]
getPapersByFilter email filt = do
  bs <- queryRawMongo $ papersByFilter email filt
  return $ catMaybes $ map paperFromBson bs

getPaperByUrl :: Text -> Url -> Handler (Maybe (PaperId,Paper))
getPaperByUrl email url = do
  mbson <- queryRawMongoOne $ paperByFilter email ["url" =: url]
  let res = case mbson of
              Just bs ->
                case ((bs !? "_id" :: Maybe ObjectId) >>= (fromPathPiece . T.pack . show),paperFromBson bs) of
                  (Just i,Just p) -> Just (i,p)
              Nothing -> Nothing
  return res




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












