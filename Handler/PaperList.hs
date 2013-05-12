-- Handler.Paper


{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Handler.PaperList where

import Import

import Yesod.Auth -- (requireAuthId')

import System.FilePath
-- import System.Random
import Data.UUID.V4

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.MessagePack as MP
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
-- import Safe

import Handler.Utils
import Handler.Widget
import Handler.Form
-- import Model.PaperReader
import Model.PaperMongo

-- import Data.Aeson hiding (object)

-- import Data.Maybe
-- import Control.Applicative
-- import Control.Monad
import Data.List (nub)

getPaperListR :: Handler RepHtml
getPaperListR = do
  -- email <- requireAuthId'
  (onetimeTokenFW, enc) <- generateFormPost onetimeTokenF
  nowrapLayout $(widgetFile "paperlist")

getPaperHistory :: [Entity Paper] -> Handler [(PaperId,Paper,Int,[History])]
getPaperHistory res = do
  email <- requireAuthId'
  forM (zip res [0..]) $ \(Entity pid p, i) -> do
    -- ToDo; You can't add PaperUserEmail ==. email for this query. Is this okay for security?
    -- This is probably OK, because multiple users don't share one pid.
    hists <- runDB $ selectList [HistoryPaper ==. Just pid] [] -- :: Handler [Entity History]
    return (pid,p,i,(map (\(Entity _ h) -> h) hists))

-- ToDo: check if this reading all papers is practical or not.
getListAllTagsR :: Handler TypedContent
getListAllTagsR = do
  email <- requireAuthId'
  alltags <- queryRawMongo $ collectTags email []
  let
    mkTagJson tag = object ["tag" .= tag, "numPapers" .= (length $ filter (tag `elem`) alltags)]
    json = toJSON (map mkTagJson (nub $ concat alltags))
  return $ toTypedContent json

getPaperListTabletR :: Handler RepHtml
getPaperListTabletR = do
  email <- requireAuthId'
  (onetimeTokenFW, enc) <- generateFormPost onetimeTokenF
  nowrapLayout $(widgetFile "paperlist_tablet")

getPaperListMobileR :: Handler RepHtml
getPaperListMobileR = do
  email <- requireAuthId'
  (onetimeTokenFW, enc) <- generateFormPost onetimeTokenF
  nowrapLayout $(widgetFile "paperlist_mobile")

getPaperListWithJsonR :: Handler RepHtml
getPaperListWithJsonR = do
  email <- requireAuthId'
  let userid = email
  nowrapLayout $(widgetFile "paperlist_with_json")

postExportAllR :: Handler (ContentType,Content) 
postExportAllR = do
  redirect HomeR

exportPapersJson :: Handler TypedContent
exportPapersJson = do
  email <- requireAuthId'
  idtext <- lookupPostParam "ids"
  let ids = splitIdText idtext
  papers <- getPapersByIds email ids
  let json = toJSON $ map toJSON papers ++ [object ["version" .= ("0.9.0"::String)]]
  return $ toTypedContent json
  where
      f :: Entity Paper -> Paper
      f (Entity pid p) = p

-- json = Array $ V.fromList (map (makeJSONFromPaper . f) papers)

getLibraryImportR = do
  (formWidget, formEnctype) <- generateFormPost fileUploadForm
  nowrapLayout $ do
      setTitle "Import Library by uoloading - PaperServer"
      [whamlet|
<h1> Import library
<form #fileupload method=post enctype=#{formEnctype} action=@{LibraryImportSubmitR}>
  ^{formWidget}
  <input type=submit>
|]

postLibraryImportSubmitR :: Handler RepHtml
postLibraryImportSubmitR = do
  email <-requireAuthId'
  ((result, formWidget), formEnctype) <- runFormPost fileUploadForm
  papers <- case result of
    FormSuccess (fi,txt) -> do
      savename <- liftIO $ randomFileName
      liftIO $ fileMove fi savename
      bsData <- liftIO $ BL.readFile savename
      return $ importMP bsData
    FormFailure ts -> do
      liftIO $ putStrLn "Error in import"
      liftIO $ print ts
      return []
  liftIO $ putStrLn $ "Papers to be added: " ++ (show $ length papers)
  ids <- runDB $ mapM insert papers
  runDB $ updateWhere [PaperId <-. ids] [PaperUserEmail =. Just email]
  redirect PaperListR

-- fileUploadForm :: AForm App App FileInfo
fileUploadForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "Name" Nothing

importMP :: BL.ByteString -> [Paper]
importMP _ = []
{-
importMP bs =
  case MP.tryUnpack bs :: Either String [MP.Object] of
      Right objs -> catMaybes $ map paperFromMsgPack objs
      Left err -> []
-}
-- http://stackoverflow.com/questions/10880105/efficient-large-file-upload-with-yesod

uploadDirectory :: FilePath -- FIXME: make this configurable
uploadDirectory = "incoming"


randomFileName :: IO FilePath
randomFileName = do
  ui <- nextRandom
  return $ uploadDirectory </> show ui
{-
randomFileName :: IO FilePath
randomFileName = do
  fname'base <- replicateM 20 (randomRIO ('a','z'))
  let fname = uploadDirectory </> fname'base
  return fname
-}
