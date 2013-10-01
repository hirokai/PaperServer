{-# LANGUAGE DeriveDataTypeable #-}

module Handler.Activity where

import Import
import Yesod.Auth
import Handler.Utils(requireAuthId')

import Model.PaperMongo

-- for Hastache
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8 
import Data.Data 
import Data.Generics

import Data.Time.Clock


data TimedEvent = TimedEvent {
  evt_citation :: Citation
  , evt_time :: UTCTime
} deriving (Data, Typeable)

data Activity = Activity {
    reading :: [TimedEvent]
    , adding :: [TimedEvent]
}  deriving (Data, Typeable)

collectActivity :: Text -> IO Activity
collectActivity email = do
  let act = Activity [] []
  return act

collectActivityJson :: Text -> Handler Value
collectActivityJson email = do
  sums <- getSummariesByFilter email []  -- Stub: all papers
  return $ object ["success" .= True, "papers" .= sums]


getActivityR :: Handler TypedContent
getActivityR = do
  email <- requireAuthId'
  template <- liftIO $ BS.readFile "templates/activity.mastache.html"
  activity <- liftIO $ collectActivity email
  res <- liftIO $ hastacheStr defaultConfig template
        (mkGenericContext activity) 
  return $ toTypedContent ("text/html" :: ByteString, toContent res)

getActivityJsonR :: Handler TypedContent
getActivityJsonR = do
  email <- requireAuthId'
  json <- collectActivityJson email
  return $ toTypedContent json



