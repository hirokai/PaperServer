{-# LANGUAGE NoMonomorphismRestriction #-}

module Handler.Form where

import Import
import Data.String

onetimeTokenF = renderDivs $ areq textField (withId "Ids" "ids" "id") Nothing

removePapersF = renderDivs $ areq textField (withId "Ids" "remove-ids" "id") Nothing

reparsePapersF = renderDivs $ areq textField (withId "Ids" "reparse-ids" "id") Nothing

addTagsF = renderDivs $ (,)
            <$> areq textField (withId "Ids" "addtag-ids" "id") Nothing
            <*> areq textField "Tags" Nothing

removeTagsF = renderDivs $ (,)
                <$> areq textField (withId "Ids" "removetag-ids" "id") Nothing
                <*> areq textField "Tags" Nothing

exportPapersF = renderDivs $ areq textField (withId "Ids" "export-ids" "id") Nothing

confirmRegisterF email = renderDivs $ User
    <$> areq emailField "Email" (Just email)
    <*> aopt textField "First name (optional)" Nothing
    <*> aopt textField "Last name (optional)" Nothing

withId :: String -> Text -> Text -> FieldSettings App
withId label id name = FieldSettings (fromString label) Nothing (Just id) (Just name) []

{-
-- signUpForm :: Html -> MForm App App (FormResult UserSignUp, Widget)
signUpForm = renderDivs $ User
    <$> areq emailField "Email" Nothing
    <*> aopt textField "First name (optional)" Nothing
    <*> aopt textField "Last name (optional)" Nothing
    -}
