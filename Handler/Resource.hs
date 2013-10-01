{-# LANGUAGE TemplateHaskell #-}

-- Handler.Resource

module Handler.Resource (
       getResourceR
       , getResourcesForPaperR
       , postUploadResourceR
       , postAttachFileR
       , getAttachmentR
  )
where

import Import
import qualified Data.Text as T
import Data.List hiding (insert)
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Ae

import Handler.Utils (requireAuthId')
import Model.PaperMongo (getPaperDB,getPaperMisc,updatePaperMisc)

-- Returns the resource list for the specified paper.
-- Client will download the image files (if not yet) and send them to server.
getResourcesForPaperR :: PaperId -> Handler TypedContent
getResourcesForPaperR pid = do
  email <- requireAuthId'
  mp <- getPaperDB email pid
  case mp of
    Just paper -> do
      let title = citationTitle $ paperCitation paper
      let urls = (map figureImg . paperFigures) paper ++
                             ((map resourceUrl . paperResources) paper)
      ids <- mapM (liftIO . resourceId) urls
      let resources = zipWith (\mi u -> object (["exists" .= isJust mi, "url" .= u] ++ maybe [] (\i -> ["id" .= i]) mi)) ids urls
      return $ toTypedContent $ object ["success" .= True, "resources" .= resources]
    Nothing -> do
      return $ toTypedContent $ object ["success" .= False, "message" .= ("ID not found."::Text)]      

resourceId :: Url -> IO (Maybe Text)
resourceId url = do
  let rid = mkFileName (T.unpack url)
  ex <- doesFileExist (resourceRootFolder ++ rid)
  return $ if ex then Just (T.pack rid) else Nothing

getResourceR :: String -> Handler () 
getResourceR hash = do
  email <- requireAuthId'
  -- liftIO $ putStrLn hash
  sendFile ctype (resourceRootFolder++hash)
    where
      -- FIXME: Currently this does not have any effect.
      ctype = case find (`isSuffixOf` hash) [".gif",".jpg",".jpeg",".png"] of
--                _ -> typeGif
                Just ".gif" -> typeGif
                Just ".jpg" -> typeJpeg
                Just ".jpeg" -> typeJpeg
                Just ".png" -> typePng
                _ -> "" -- Not supported yet.

-- ToDo: we need a mechanism to avoid overwriting by wrong data by unknown users,
-- probably by separating users.
postUploadResourceR :: Handler TypedContent
postUploadResourceR = do
  email <- requireAuthId'
  mtpid <- lookupPostParam "id"
  let mpid = case mtpid of
               Nothing -> Nothing
               Just tpid -> fromPathPiece tpid
  mftype <- lookupPostParam "type"
  murl <- lookupPostParam "url"
  mdat <- lookupPostParam "data"
  $(logInfo) $ "postUploadResourceR: bytes: " `T.append` maybe "N/A" (T.pack . show . T.length) mdat
  obj <- case (mpid,mftype,murl,mdat) of
           (Just pid, Just ftype, Just url, Just dat) ->
             saveUploadedImg pid ftype url dat
           _ -> return $ object ["success" .= False, "message" .= ("Params missing."::String)]
  return $ toTypedContent $ toJSON obj

saveUploadedImg :: PaperId -> Text -> Text -> Text -> Handler Value
saveUploadedImg pid ftype url dat = do
  email <- requireAuthId'
  let
    file = imageCachePath url
    dec = B64.decode (encodeUtf8 dat)
  case dec of
    Left err -> do
      -- $(logInfo) $ T.pack err
      return $ object ["success" .= False, "message" .= ("Base64 decode error." :: String)]
    Right bs -> do
      liftIO $ B.writeFile file bs
      let img = Resource (T.pack $ mkFileName $ T.unpack url) url ftype file
      rid <- runDB $ insert img
      return $ object ["success" .= True, "resource_id" .= rid]   -- "resource_id" is a DB id, not a resId field.
 

postAttachFileR :: PaperId -> Handler TypedContent
postAttachFileR pid = do
  email <- requireAuthId'
  mdat <- lookupPostParam "data"
  json <- case mdat of
    Just dat -> do
      let bin = encodeUtf8 (T.drop 7 $ snd $ T.breakOn "base64," dat)
      case B64.decode bin of -- Stub: Ad hoc: Drops "data:*****;base64," 
        Right d -> doAttachFile email pid d
        Left err -> return $ object ["success" .= False, "message" .= ("Base64 decode failed: " ++ err)]
    Nothing -> return $ object ["success" .= False, "message" .= ("No data found." :: Text)]
  return $ toTypedContent json

getAttachmentR :: Text -> Handler TypedContent
getAttachmentR resId = do
  email <- requireAuthId'
  sendFile "application/pdf" (attachmentFolder ++ (T.unpack $ resId))
  -- Stub: Assuming PDF.

doAttachFile :: Text -> PaperId -> ByteString -> Handler Value
doAttachFile email pid dat = do
  mval <- getPaperMisc email pid
  datid <- return (toPathPiece pid)  -- Stub
  saveAttachment pid datid dat
  let key = "attachedFile"
  let newobj = case mval of
                 (Just (Ae.Object obj)) -> HashMap.insert key (Ae.String datid) obj
                 _ -> HashMap.singleton key (Ae.String datid) :: HashMap.HashMap Text Value
  success <- updatePaperMisc email pid (Ae.Object newobj)
  return $ case success of 
    True -> object ["success" .= True]
    False -> object ["success" .= False, "message" .= ("Database error"::Text)]

saveAttachment :: PaperId -> Text -> ByteString -> Handler Bool
saveAttachment pid datid dat = do
  liftIO $ B.writeFile (attachmentFolder ++ (T.unpack $ toPathPiece pid)) dat -- stub
  return True


