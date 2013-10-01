{-# LANGUAGE NoMonomorphismRestriction #-}

-- Handler.Utils

module Handler.Utils where

import Import

import Yesod.Auth (maybeAuthId)

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Digest.Pure.SHA (sha256,showDigest)
import Data.ByteString.Lazy (fromChunks)

import Model.PaperMongo (getPaperByFilter)

import qualified Database.MongoDB as DB
import Data.Bson ((!?))

import Database.Persist.MongoDB

-- requireAuthId' :: YesodAuthPersist master => HandlerT master IO (AuthId master)
requireAuthId' = maybeAuthId >>= maybe redirectLogin' return

redirectLogin' :: Yesod master => HandlerT master IO a
redirectLogin' = do
    y <- getYesod
    setUltDestCurrent
    case authRoute y of
        Just z -> redirect z
        Nothing -> permissionDenied "Please configure authRoute"
--
-- General utility functions
--

mapT3 :: ((a->b),(a->c),(a->d))->a->(b,c,d)
mapT3 (f,g,h) a = (f a, g a, h a)

mapT4 :: ((a->b),(a->c),(a->d),(a->e))->a->(b,c,d,e)
mapT4 (f,g,h,i) a = (f a, g a, h a,i a)

zip2d :: [[a]] -> [[b]] -> [[(a,b)]]
zip2d a b = map (\x -> zip (fst x) (snd x)) (zip a b)


--
-- For DB access.
--
-- always use this instead of selectList to avoid fetching other users' papers.

-- ToDo: Use raw mongo to avoid overhead whenever appropriate.
-- selectListUser email cond filt = selectList ([PaperUserEmail ==. Just email] ++ cond) filt

--
-- For rendering papers
--


--ToDo: check this separator. Make a tree structure, instead offlat, linear.
decodeToc :: Paper -> [Text]
decodeToc = const [] -- maybe [] (T.splitOn "\t") . paperToc

doi2url :: Text -> String
doi2url doi = "http://dx.doi.org/" ++ T.unpack doi


-- assumes cr is not used for id.
splitIdText :: Maybe Text -> [PaperId]
splitIdText mtext = fromMaybe [] $ fmap (catMaybes . map fromPathPiece . T.splitOn "\n") mtext

splitParamText :: Maybe Text -> [Text]
splitParamText mtext =
  let
    tok = fmap (T.splitOn "\n") mtext
  in
    case tok of
      Nothing -> []
      Just [""] -> []
      Just xs -> xs

localRes :: Url -> String
localRes u = "/resource/"++ getResourceId u

-- fromStrict :: ByteString -> BL.ByteString
fromStrict = fromChunks . (:[])

getResourceId :: Url -> String
getResourceId = filter (/= ' ') . showDigest . sha256 . fromStrict . encodeUtf8

refToDoi :: Reference -> T.Text
refToDoi ref = fromMaybe "" (referenceCit ref >>= citationDoi)


--
-- Links in reference section
--

data LinkStatus = Available | Unavailable | Downloaded

getLinkStatus :: [Reference] -> Handler [LinkStatus]
getLinkStatus refs = 
  mapM status refs
  where
    status :: Reference -> Handler LinkStatus
    status (Reference _ _ (Just cit) _ _) = do
        res <- case doi of
                  Just d -> do
                    me <- maybeAuthId
                    case me of
                      Just e ->
                        getPaperByFilter e ["doi" =: d]
                      Nothing -> return Nothing
                  Nothing -> return Nothing
        return $ case res of
                Just paper -> Downloaded
                Nothing -> Unavailable -- Stub
 {-               _ -> case url >>= PR.readerFromUrl of
                       Just _ -> Available
                       Nothing -> Unavailable -}
        where
          doi = citationDoi cit
          url = citationUrl cit
    status _ = return Unavailable -- Stub, may use samePaper(expensive, though)

linkStatus :: Reference -> LinkStatus
linkStatus r = Unavailable -- stub
{-
linkStatus r = case url >>= PR.readerFromUrl of
                  Just _ -> Available
                  Nothing -> Unavailable -}
  where
    doi,url :: Maybe Text
    doi = referenceCit r >>= citationDoi
    url = referenceCit r >>= citationUrl

linkIcon,linkClass :: LinkStatus -> T.Text
linkIcon Available = "/static/img/jump_available.png"
linkIcon Unavailable = "/static/img/jump_unavailable.png"
linkIcon Downloaded = "/static/img/jump_ready.png"

linkClass Available = "link-available"
linkClass Unavailable = "link-unavailable"
linkClass Downloaded = "link-ready"


deleteWhereUser email cond =  deleteWhere (cond++[PaperUserEmail ==. Just email])

-- Insert with a specified paperUserMail
insertUser email paper =  insert (paper{paperUserEmail=Just email})

-- insertUserH :: Text -> History -> Handler HistoryId
insertUserH email (History pid action time _ info)
  = insert (History pid action time (User email Nothing Nothing) info)

-- replaceUser :: Text -> PaperId -> Paper -> Handler PaperId
replaceUser email pid paper = replace pid (paper{paperUserEmail = Just email})


{-
runRawMongo :: Handler ()
runRawMongo = do
  app <- getYesod
  let dbconf = persistConfig app
  pipe <- liftIO $ DB.runIOE $ DB.connect (DB.host $ T.unpack (mgHost dbconf))
  DB.access pipe DB.ReadStaleOk "PaperServer" $ do
    e <- allSummariesOfUser ("hiroyuki.kai@gmail.com" ::Text)
    liftIO $ print e
    liftIO $ print $ map fromBson e
  liftIO $ DB.close pipe
  return ()
-}




historyPaper :: Text -> PaperId -> Handler [History]
historyPaper email pid = do
  ents <- runDB $ selectList [HistoryPaper ==. (Just pid), HistoryUser ==. User email Nothing Nothing] []
  return $ map f ents
  where
    f (Entity hid hist) = hist

{-
paperAddedDate :: Text -> PaperId -> Handler (Maybe History)
paperAddedDate email pid = do
  me <- runDB $ selectFirst [HistoryAction ==. HACreate, HistoryPaper ==. (Just pid),
            HistoryUser ==. User email Nothing Nothing] []
  return $ fmap f me
  where
    f (Entity hid h) = h
-}
