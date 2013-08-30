{-# LANGUAGE DoAndIfThenElse,TemplateHaskell #-}
--
-- Handler.PaperListW2UI
-- Returns JSON that is specific to w2ui library.
--


module Handler.PaperListW2UI where


import Import
import qualified Data.Text as T
import Safe

import Handler.Utils
import Model.PaperReader
import Model.PaperMongo

import Database.MongoDB ((=:))
import qualified Database.MongoDB as DB

import Data.Bson ((!?))
import qualified Data.Bson as Bson
import qualified Data.HashMap.Strict as HM
import Data.Attoparsec.Number (Number(..))

-- Returns all papers
getPaperListJsonR :: Handler TypedContent
getPaperListJsonR = do 
  email <- requireAuthId'
  sums <- getSummariesByQuery (DB.select ["user_email" =: email] "paper"){DB.project = summaryList}
  return $ toTypedContent $ toJSON sums


-- This returns JSON for w2ui library.
-- FIXME: parse the arguments correctly.
postPaperListJsonR = do
  email <- requireAuthId'
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

-- ToDo: Make this clearer.
-- Fcuntions from Persistent modules may be incompetent for my purpose.
-- Look at this for implementing more elaborated queries.
-- https://github.com/yesodweb/yesod/wiki/Rawmongo
-- http://hackage.haskell.org/packages/archive/mongoDB/0.4.2/doc/html/Database-MongoDB.html
-- Using the parsed request, return JSON.
sendGridJson :: Maybe W2UIGridReq -> Handler TypedContent
sendGridJson mreq = do
  email <- requireAuthId'
  total <- countPapersByQuery (mkSearch mreq (defaultQuery email))
  sums <- getSummariesByQuery (mkSearch mreq . mkFilter mreq $ defaultQuery email)
  let per_page = maybe 50 limit mreq
  let off = maybe 0 offset mreq 
  let
    addRecId :: Int -> PaperSummary -> Value
    addRecId n sum =
      let
        (Object obj) = toJSON sum
      in
        Object $ HM.insert "recid" (Number (I (fromIntegral n))) obj
    json = object [
                "total" .= total
                , "page" .= (off `div` per_page)  --Stub: 20 should be customizable.
                , "records" .= (zipWith addRecId [off+1..] sums)
                ]
  return $ toTypedContent json

{-
addDate :: Text -> PaperSummary -> Handler PaperSummary
addDate email (PaperSummary t c pid d tags avail date)  = do
  mh <- case fromPathPiece pid of
          Just t -> paperAddedDate email t
          Nothing -> return Nothing
  return $ PaperSummary t c pid d tags avail (maybe date (T.pack . show . historyTime) mh)

-}


-- http://w2ui.com/web/docs/grid


-- This returns JSON for w2ui library.
-- ToDo: Move this to a right module.

summaryEx' :: Entity Paper -> Handler [(Text, Value)]
summaryEx' (Entity pid p) = summaryEx pid p

summaryEx :: PaperId -> Paper -> Handler [(Text, Value)]
summaryEx pid p = do
  let
    base = paperSummary pid p
    abs = isJust $ paperAbstract p
    full = isJust $ paperMainHtml p
    fig = not . null $ paperFigures p
    ref = not . null $ paperReferences p
    added = paperTimeAdded p
  let obj = object ["abstract".= abs, "fulltext" .= full, "figures" .= fig, "references" .= ref]
  return $ base ++ ["available" .= obj, "date" .= added]

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

mkSearch :: Maybe W2UIGridReq -> DB.Query -> DB.Query
mkSearch Nothing = id
mkSearch (Just (W2UIGridReq _ _ _ search _)) = foldl (.) id $ catMaybes $ map mk search
  where
    mk (W2UISearch f b c v) =
      let
        vr = ["$regex" =: v]
        fields = map (\s -> [s =: vr]) ["citation.title","abstract","main_html","citation.journal"]
      in
        case (f,v) of
          ("tags","") -> Nothing
          ("tags",_) -> Just $ addSelect ["tags" =: ["$all" =: (T.splitOn "\n" v)]]
          (_,_) ->
            Just $ addSelect ["$or" =: fields]

addSelect filt q = q{DB.selection=DB.Select (DB.selector (DB.selection q)++filt) "paper"}

-- http://hackage.haskell.org/packages/archive/mongoDB/1.3.1/doc/html/Database-MongoDB-Query.html

-- This is for filtering and sorting MongoDB query.
-- ToDo: This currently assumes "and" for multiple conditions.
-- myparseJson gets only the first one now.
mkFilter :: Maybe W2UIGridReq -> DB.Query -> DB.Query
mkFilter Nothing = id
mkFilter (Just (W2UIGridReq limN offset c search sorts))
  = filtering . sorting
  where
    d :: Text -> Int
    d "asc" = 1 
    d "desc" = -1 
    d _ = 0
    filtering, sorting :: DB.Query -> DB.Query
    filtering q =
      q{DB.limit=(fromIntegral limN),DB.skip=(fromIntegral offset)}  -- Stub!! Add keyword search
    sorting q =
      case sorts of
        [] -> q
        (W2UISort "id" dir:_) ->
          q{DB.sort = ["_id" =: d dir]}
        (W2UISort "citation.title" dir:_) ->
          q{DB.sort = ["citation.title" =: d dir]}
        (W2UISort "cittext" dir:_) ->
          q{DB.sort = ["citation.journal" =: d dir]}   -- Stub!!
        (W2UISort "available.abstract" dir:_) ->
          q{DB.sort = ["abstract" =: d dir]}   -- Stub!! This sorts also based on the content of abstract, which is wrong.
        (W2UISort "available.fulltext" dir:_) ->
          q{DB.sort = ["support_level" =: d dir]}   -- Stub!! 
        (W2UISort "available.references" dir:_) ->
          q{DB.sort = ["support_level" =: d dir]}   -- Stub!! 
        (W2UISort "available.figures" dir:_) ->
          q{DB.sort = ["support_level" =: d dir]}   -- Stub!! 
        -- (W2UISort "tags" dir:_) ->
        --  "" -- not implemented yet
        (W2UISort "date" dir:_) ->
          q{DB.sort = ["time_added" =: d dir]}
        (W2UISort "citation.type" dir:_) ->
          q{DB.sort = ["citation.type" =: d dir]}
        _ -> q{DB.sort = ["time_added" =: (-1::Int)]}

