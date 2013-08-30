{-# LANGUAGE TemplateHaskell #-}

-- Handler.Resource

module Handler.Resource (
       getResourceR
       , getResourceForPaperR
       , postUploadResourceR
  )
where

import Import
import qualified Data.Text as T
import Data.List hiding (insert)
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding


-- Returns the resource list for the specified paper.
-- Client will download the image files (if not yet) and send them to server.
getResourceForPaperR :: PaperId -> Handler TypedContent
getResourceForPaperR pid = do
  paper <- runDB $ get404 pid
  let title = citationTitle $ paperCitation paper
  obj <- if title == Nothing then
              return $ object ["success" .= False, "message" .= ("Parsing not yet done." :: String)]
            else do
              let urls = (map figureImg . paperFigures) paper ++
                                     ((map resourceUrl . paperResources) paper)
              new_urls <- liftIO $ filterM (fmap not . resourceExists) urls
              $(logInfo) $ T.pack $ show $ Data.List.length new_urls
              return $ object ["success" .= True, "url" .= new_urls]
  return $ toTypedContent $ toJSON obj

resourceExists :: Url -> IO Bool
resourceExists url = doesFileExist (resourceRootFolder ++ mkFileName (T.unpack url))

getResourceR :: String -> Handler () 
getResourceR hash = do
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
 

