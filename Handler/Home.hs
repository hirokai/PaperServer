{-# LANGUAGE TupleSections, OverloadedStrings, DoAndIfThenElse #-}
module Handler.Home where

import Import
import Yesod.Auth
import Handler.Widget
import Handler.Form
import Handler.Utils(requireAuthId')
import Data.FileEmbed

-- import Data.ByteString(ByteString)
import Data.Text.Encoding (decodeUtf8)

import Data.List (find)
-- import Data.Maybe
import qualified Data.ByteString as BS
import Network.Wai

import Text.Blaze.Html (preEscapedToHtml)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.


optionsAuthR :: Handler RepPlain
optionsAuthR = do
    -- ToDo: Add other sites.
    setHeader "Access-Control-Allow-Origin" "http://pubs.acs.org"
    setHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
    return $ RepPlain $ toContent ("" :: Text)

getHomeR :: Handler RepHtml
getHomeR = do
  mu <- maybeAuthId
  mobile <- isMobile
  redirect $
    case (mu,mobile) of
      (Just user,False) -> PaperListR
      (Just user,True) -> PaperListMobileR
      (Nothing,False) -> WelcomeR
      (Nothing,True) -> WelcomeMobileR

getStartR :: Handler RepHtml
getStartR = do
  mobile <- isMobile
  redirect $ if mobile then PaperListMobileR else PaperListR


isMobile :: Handler Bool
isMobile = do
  req <- waiRequest
  let hs = requestHeaders req
  let mua = fromMaybe ("","") $ find (\(k,v) -> k == "User-Agent") hs
  return $ any (\s -> s `BS.isInfixOf` (snd mua)) ["Android","iPhone","iPad","iPod"]
  
getWelcomeR :: Handler RepHtml
getWelcomeR = nowrapLayout $(widgetFile "welcome")

getWelcomeMobileR :: Handler RepHtml
getWelcomeMobileR = nowrapLayout $(widgetFile "welcome_mobile")

getSignUpR :: Handler RepHtml
getSignUpR = do
  email <- requireAuthId'
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

getUnregisteredR :: Handler RepHtml
getUnregisteredR = nowrapLayout $(widgetFile "not_registered_user")


postConfirmSignUpR :: Handler RepHtml
postConfirmSignUpR = do
  ((result, widget), enctype) <- runFormPost (confirmRegisterF "")
  case result of
    FormSuccess user@(User _email first last) -> do
      email <- maybeAuthId
      if email == Just _email then do
        runDB $ insert user
        redirect PaperListR
      else
        redirect WelcomeR
    _ -> redirect WelcomeR


policyText :: Text
policyText = decodeUtf8 $(embedFile "config/policy_embed.html")
