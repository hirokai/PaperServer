{-# LANGUAGE TupleSections, OverloadedStrings, DoAndIfThenElse #-}
module Handler.Home where

import Import
import Yesod.Auth
import Handler.Widget
import Handler.Form
import Data.FileEmbed

import Data.ByteString(ByteString)
import Data.Text.Encoding (decodeUtf8)

import Text.Blaze.Internal (preEscapedText)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getHomeR :: Handler RepHtml
getHomeR = do
  mu <- maybeAuthId
  case mu of
    Just user -> redirect PaperListR
    Nothing -> redirect WelcomeR

getWelcomeR :: Handler RepHtml
getWelcomeR = nowrapLayout $(widgetFile "welcome")

getSignUpR :: Handler RepHtml
getSignUpR = do
  email <- requireAuthId
  -- (widget, enctype) <- generateFormPost signUpForm
  muser <- runDB $ getBy (UniqueUser email)
  allowed <- isThisUserAllowed email
  if allowed then
    case muser of
      Just _ -> do
        nowrapLayout $(widgetFile "duplicated_user")
      Nothing -> do
        (confirmRegisterFW, enctype) <- generateFormPost (confirmRegisterF email)
        nowrapLayout $(widgetFile "confirm_register")
  else
    nowrapLayout $(widgetFile "register_suspended")

-- This is used for controlling the number of new registration users.
-- This returns True by default.
isThisUserAllowed :: Text -> Handler Bool
isThisUserAllowed _ = return True


postConfirmSignUpR :: Handler RepHtml
postConfirmSignUpR = do
  ((result, widget), enctype) <- runFormPost (confirmRegisterF "")
  case result of
    FormSuccess user@(User _email first last) -> do
      email <- requireAuthId
      if email == _email then do
        runDB $ insert user
        redirect PaperListR
      else
        redirect WelcomeR
    _ -> redirect WelcomeR


policyText :: Text
policyText = decodeUtf8 $(embedFile "config/policy_embed.html")
