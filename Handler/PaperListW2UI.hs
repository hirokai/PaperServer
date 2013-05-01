{-# LANGUAGE DoAndIfThenElse #-}
--
-- Handler.PaperListW2UI
-- Returns JSON that is specific to w2ui library.
--


module Handler.PaperListW2UI where

import Import

import Yesod.Auth -- (requireAuthId)

import System.FilePath
import System.Random

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Safe

import Handler.Utils
import Handler.Widget
import Handler.Form
import Model.PaperReader

import Data.Aeson hiding (object)

import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.List (sortBy,nub)

import Database.MongoDB((=:))
import Database.MongoDB (Document, Action, findOne)
import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB

getPaperListJsonR :: Handler RepJson
getPaperListJsonR = sendGridJson Nothing


-- This returns JSON for w2ui library.
-- ToDo: parse the arguments correctly.
postPaperListJsonR = do
  email <- requireAuthId
  body <- runRequestBody
  -- liftIO $ print $ fst body
  -- rreq <- parseJsonBody
  -- liftIO $ print rreq
  -- let mreq = case rreq of
  --             Success r -> Just r
  --             Error _ -> Nothing
  -- backup <- myparseJson
  -- sendGridJson (mreq <|> backup)
  json <- myparseJson
  sendGridJson json

-- This is adhoc.
-- ToDo: figure out how to parse this in a standardized way.
-- http://w2ui.com/web/docs/grid
-- Currently only single sort and sigle search is implemented.
myparseJson :: Handler (Maybe W2UIGridReq)
myparseJson = do
  offp <- lookupPostParam "offset"
  limp <- lookupPostParam "limit"
  searchp <- lookupPostParam "search-logic"
  let lim =  limp >>= (readMay . T.unpack)
  let off =  offp >>= (readMay . T.unpack)
  rf <- lookupPostParam "search[0][field]"
  rt <- lookupPostParam "search[0][type]"
  ro <- lookupPostParam "search[0][operator]"
  rv <- lookupPostParam "search[0][value]"
  sd <- lookupPostParam "sort[0][direction]"
  sf <- lookupPostParam "sort[0][field]"
  sd <- lookupPostParam "sort[0][direction]"
  -- field": "fname", "dataType": "text", "value": "vit", "operator": "is" },
  let res = W2UIGridReq <$> lim <*> off <*> searchp
                        <*> (Just (maybeToList $ W2UISearch <$> rf <*> rt <*> ro <*> rv))
                        <*> Just (maybeToList $ (W2UISort <$> sf <*> sd))
  $(logInfo) $ T.pack $ show res
  return res
{-
summaryFromDB :: Text -> Handler [PaperSummary]
summaryFromDB email = do
  cur <- MongoDB.find (MongoDB.select ["user_email" =: email] "PaperServer"){MongoDB.project = ["citation" =. 1, "_id" =. 1]}
  allrs <- MongoDB.rest cur
  return (catMaybes $ map mkSummary allrs)
  where
    mkSummary bson = Nothing

-}
data PaperSummary = PaperSummary {
  pTitle :: Text,
  pCitText :: Text,
  pId :: Text,
  pDoi :: Text,
  pTags :: [Text]
}

-- ToDo: Make this clearer.
-- Fcuntions from Persistent modules may be incompetent for my purpose.
-- Look at this for implementing more elaborated queries.
-- https://github.com/yesodweb/yesod/wiki/Rawmongo
-- http://hackage.haskell.org/packages/archive/mongoDB/0.4.2/doc/html/Database-MongoDB.html
-- Using the parsed request, return JSON.
sendGridJson :: Maybe W2UIGridReq -> Handler RepJson
sendGridJson mreq = do
  email <- requireAuthId
  liftIO $ print mreq
  let num = maybe 50 limit mreq
  let search = maybe [] mkSearch mreq
  let cond = [LimitTo num] ++ maybe [] mkFilter mreq
  res_all <- runDB $ selectListUser email [] []
  -- res <- runDB $ selectListUser email search cond
  res_filtered <- filterM (maybe (const (return True)) myFilter mreq) res_all
  sort_key <- case mreq of
                Nothing -> return $ map (T.pack . show) [1..]  -- No sort.
                Just req -> mapM (mySortKey req) res_filtered
                -- ToDo: support both asc and desc
  let res_sorted' = map snd $ sortBy (\a b -> compare (fst a) (fst b)) (zip sort_key res_filtered)
  let res_sorted = case mreq of
                     Just (W2UIGridReq _ _ _ _ (W2UISort _ "desc":_)) -> reverse res_sorted'
                     _ -> res_sorted'
  let off = maybe 0 offset mreq
  summ <- mapM summaryEx' $ drop off res_sorted 
  let
    addRecId :: Int -> [(Text,Value)] -> Value
    addRecId n ps = object (["recid" .= n] ++ ps)
    json = object [
                "total" .= (length res_sorted)
                , "page" .= (off `div` num)  --Stub: 20 should be customized.
                , "records" .= take num (zipWith addRecId [off+1..] summ)
                ]
  jsonToRepJson json


-- You need to further filter the results manually,
-- because selectList does not seem able to search title, citation, date, etc.
-- Seems that currenly there is no filter usable.

--This is for Yesod.Persistent
mkSearch (W2UIGridReq _ _ _ search _) = catMaybes $ map mk search
  where
    mk (W2UISearch a b c d) = Nothing

-- This is for "manual" (or app side) filtering.
-- ToDo: This currently assumes "and" for multiple conditions.
-- myparseJson gets only the first one now.
myFilter :: W2UIGridReq -> Entity Paper -> Handler Bool
myFilter (W2UIGridReq a b c search e) ent@(Entity pid p) = do
  case search of
    [] -> return True
    (W2UISearch "tags" _ "is" tagtxt:preds) -> do
      if tagtxt == "" then
        myFilter (W2UIGridReq a b c preds e) ent
      else do
        let st = T.splitOn "\n" tagtxt
        let pt = paperTags p
        if all (\t -> t `elem` pt) st then
          myFilter (W2UIGridReq a b c preds e) ent
        else
          return False

mySortKey :: W2UIGridReq -> Entity Paper -> Handler Text
mySortKey (W2UIGridReq _ _ _ _ sorts) ent@(Entity pid p) = do
  let res = case sorts of
              [] -> ""
              (W2UISort "id" dir:_) ->
                toPathPiece pid
              (W2UISort "title" dir:_) ->
                fromMaybe "" $ citationTitle (paperCitation p)
              (W2UISort "cittxt" dir:_) ->
                mkCitText (paperCitation p)
              (W2UISort "abstract_available" dir:_) ->
                if isJust (paperAbstract p) then "B" else "A"
              (W2UISort "fulltext_available" dir:_) ->
                if isJust (paperMainHtml p) then "B" else "A"
              (W2UISort "refs_available" dir:_) ->
                if not $ null (paperReferences p) then "B" else "A"
              (W2UISort "figs_available" dir:_) ->
                if not $ null (paperFigures p) then "B" else "A"
              -- (W2UISort "tags" dir:_) ->
              --  "" -- not implemented yet
              (W2UISort "type" dir:_) ->
                fromMaybe "" $ citationType $ paperCitation p
              _ -> ""
  return res


-- http://w2ui.com/web/docs/grid
-- getFilter :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m) => W2UIGridReq -> [Filter val]
-- Stub! ToDo: Make search and sort possible.
mkFilter (W2UIGridReq limit offset logic search sorts) =
  let
    s = maybe [] (\(W2UISort field direction) -> [(f direction) (g field)]) (headMay sorts)
    f "ASC" = Asc
    f "DESC" = Desc
    f _ = Asc
    g _ = PaperDoi   --Stub: No way to sort by paper title, etc in Persist.
  in
    [OffsetBy offset] ++ [] -- Stub!!


-- This returns JSON for w2ui library.
-- ToDo: Move this to a right module.

summaryEx' :: Entity Paper -> Handler [(Text, Value)]
summaryEx' (Entity pid p) = summaryEx pid p

summaryEx :: PaperId -> Paper -> Handler [(Text, Value)]
summaryEx pid p = do
  let base = paperSummary pid p
  abs <- liftIO $ resourceReady p RAbs
  full <- liftIO $ resourceReady p RFull
  let
    fig = not . null $ paperFigures p
    ref = not . null $ paperReferences p
  let obj = object ["abstract".= abs, "fulltext" .= full, "figures" .= fig, "references" .= ref]
  return $ base ++ ["available" .= obj]

data W2UISearch = W2UISearch Text Text Text Text deriving Show
data W2UISort = W2UISort Text Text deriving Show -- stub!!
-- data W2UISortDirection = SortAsc | SortDesc

data W2UIGridReq = W2UIGridReq {
  limit :: Int
  , offset :: Int
  , searchlogic :: Text
  , search :: [W2UISearch]
  , sorts :: [W2UISort]
} deriving Show

{-
instance FromJSON W2UIGridReq where
  parseJSON (Object v) =
    W2UIGridReq <$> (v .: "limit") <*> (v .: "offset") <*> pure "" -- (v .: "search-logic")
                <*> pure [(W2UISearch "" "" "" "")] -- (v .: "search")
                <*> pure [(W2UISort "" "")] -- (v .: "sort")

instance FromJSON W2UISearch where
  parseJSON (Object v) =
    return $ W2UISearch "" "" "" ""
    --W2UISearch <$> (v .: "field") <*> (v .: "dataType") <*> (v .: "value") <*> (v .: "operator")

instance FromJSON W2UISort where
  parseJSON (Object v) =
    return $ W2UISort "" "" 
  --  W2UISort <$> (v .: "field") <*> (v .: "direction")

-}
