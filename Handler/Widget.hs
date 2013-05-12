module Handler.Widget where

import Import

import qualified Data.Text as T
-- import Data.Maybe
-- import Handler.Utils
-- import Model.Defs
import Model.PaperReader

-- import Model.PaperReader.Utils (mkCitText)
import Data.List

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- import Control.Monad

-- import Model.PaperP as P
import qualified Parser.Paper as P 
import Control.Lens
-- import Text.HTML.SanitizeXSS (sanitize)

citationWidget cit = [hamlet|
<p #citation>
  #{T.intercalate ", " (citationAuthors cit)}
  <br>
  <i> #{fromMaybe "" $ citationJournal cit},
  <b> #{fromMaybe "" $ citationVolume cit},
  #{fromMaybe "" $ citationPageFrom cit} &dash;
  #{fromMaybe "" $ citationPageTo cit}
  $maybe year <- citationYear cit
    &nbsp; (#{year})
|]

referencesWidgetFormatB :: [Reference] -> Widget
referencesWidgetFormatB refs = do
  [whamlet|
<div .references>
  <h3 #references> References
  <ul .references>
    $forall ref <- refs
      <li ##{referenceRefId ref}>
        <a href=#{fromMaybe "" $ referenceUrl ref}>
          <span .refname>
            #{referenceRefName ref}
        #{fromMaybe (maybe "" mkCitText (referenceCit ref)) (referenceCitText ref)}
|]

paperInfoModalPlaceHolder :: Widget
paperInfoModalPlaceHolder = do
  $(widgetFile "paperinfo_modal_placeholder")

paperInfoModalWithData :: PaperId -> Paper -> Widget
paperInfoModalWithData paperId paper = do
  let cit = paperCitation paper
  let pages = (fromMaybe "N/A" $ citationPageFrom cit) `T.append` (maybe "" (T.append "-") $ citationPageTo cit)
  let authors = T.intercalate "; " $ citationAuthors cit
  let refs = paperReferences paper
  $(widgetFile "paperinfo_modal_withdata")

paperInfoAjax :: Paper -> Widget
paperInfoAjax paper = do
  let cit = paperCitation paper
  let pages = (fromMaybe "N/A" $ citationPageFrom cit) `T.append` (maybe "" (T.append "-") $ citationPageTo cit)
  let authors = T.intercalate "; " $ citationAuthors cit
  let refs = paperReferences paper
  $(widgetFile "paperinfo_modal_ajax")

-- nowrapLayout :: GWidget s App () -> GHandler s App RepHtml
nowrapLayout widget = do
    pc <- widgetToPageContent widget
    -- This is a bit strange, but somehow pageBody returns a whole html.
    hamletToRepHtml [hamlet|
$doctype 5
  ^{pageBody pc}
  #{preEscapedToHtml analyticsCode}
|]

paperTagsHtml :: PaperId -> [Text] -> Html
paperTagsHtml pid tags = H.div $ do
  forM_ tags $ \tag -> do
    H.span ! A.id (toValue (T.append "tag-" tag)) $ do
      toHtml tag
      let str = T.concat ["removeTag('",toPathPiece pid, "','" , tag, "');"]
      H.a ! class_ "remove-tag" ! href "" ! customAttribute "ng-click" (toValue str) $ do
        toHtml ("x" :: String)

paperListEntryWidget :: (PaperId, Paper, Int, [History]) -> Widget
paperListEntryWidget (pid, p, index, hists) = do
--  let pid = paperId p
  let c = paperCitation p
  let fm = fromMaybe ""
  let addedTime = maybe "N/A" (show . historyTime) (find (\h -> historyAction h == HACreate) hists)
  [whamlet|
<tr ng-class="classForRow(papers[#{index}])" .paper-entry paper-id="#{toPathPiece pid}" paper-doi="#{paperDoi p}" id="paper-entry-#{index}">
  <td .entry-select ng-click="papers[#{index}].selected = !papers[#{index}].selected; $scope.apply();">
    <input .check-entry type="checkbox" ng-model="papers[#{index}].selected">
    <span> #{index + 1}
    <i class="icon-chevron-right cursor-icon {{cursorClass(#{index})}}">
  <td .entry-title .span3 ng-click="papers[#{index}].selected = !papers[#{index}].selected; $scope.apply();">
    #{preEscapedToHtml $ fromMaybe "(No title)" $ citationTitle c}
  <td .entry-citation .span1  ng-click="papers[#{index}].selected = !papers[#{index}].selected; $scope.apply();">
    <i> #{fromMaybe "" $ citationJournal c},
    <b> #{fm $ citationVolume c},
    #{fm $ citationPageFrom c} &dash; #{fm $ citationPageTo c} (#{maybe "" show $ citationYear c})
  <td>
    <a href=@{PaperRb pid}>
      <button .btn .btn-small>
        <i .icon-play>
    <div .btn-group>
      <button .btn .btn-small .dropdown-toggle data-toggle="dropdown" href="#">
        <i class="icon-cog">
        <span .caret>
      <ul .dropdown-menu>
        <li>
         <a href="#" onclick="paper_delete(['#{toPathPiece pid}']);"> Delete
        <li>
         <a href="#" onclick="reparse(['#{toPathPiece pid}']);"> Reparse
        <li>
          <a href="#{fromMaybe "#" $ citationUrl c}"> Original URL
        <li>
          <a href=@{RawHtmlR pid}> Raw HTML
  <td .labelcell>
    <div style="width:100px;">
      <div .tags>
        #{paperTagsHtml pid (paperTags p)}
      <form ng-submit="addTag('#{toPathPiece pid}')">
        <input .action-addtag type="text" placeholder="Add tag..." ng-model="papers[#{index}].tagText" ng-focus="taginput_focus()" ng-focusout="taginput_focusout()">
  <td>
    #{addedTime}
|]

paperListEntryWidgetTablet :: (PaperId, Paper, Int, [History]) -> Widget
paperListEntryWidgetTablet (pid, p, index, hists) = do
--  let pid = paperId p
  let c = paperCitation p
  let fm = fromMaybe ""
  let addedTime = maybe "N/A" (show . historyTime) (find (\h -> historyAction h == HACreate) hists)
  [whamlet|
<tr .paper-entry paper-id="#{toPathPiece pid}" paper-doi="#{paperDoi p}" id="paper-entry-#{index}">
  <td>
    <input .check-entry type="checkbox" ng-model="papers[#{index}].selected">
    <span> #{index + 1}
    <i class="icon-chevron-right cursor-icon {{cursorClass(#{index})}}">
  <td .entry-title .span3>
    #{fromMaybe "(No title)" $ citationTitle c}
  <td .entry-citation .span1>
    <i> #{fromMaybe "" $ citationJournal c},
    <b> #{fm $ citationVolume c},
    #{fm $ citationPageFrom c} &dash; #{fm $ citationPageTo c} (#{maybe "" show $ citationYear c})
  <td>
    <a href=@{PaperTabletRb pid}>
      <button .btn .btn-small>
        <i .icon-play>
  <td>
    #{addedTime}
|]


-- http://www.softel.co.jp/blogs/tech/archives/2970
-- Have CDN and alternative local files.
addJS :: Text -> Text -> Text -> [Html]
addJS url local cond =
  [H.script ! A.src (toValue url) $ do toHtml (""::String),
   H.script $
     preEscapedToHtml $ T.concat [cond," && document.write('<script src=\"",local,"\"><\\/script>')"]
    ]

addJSLibraries =
  addJS "http://code.jquery.com/jquery-1.9.1.min.js" "/static/js/jquery-1.7.2.min.js" "!window.jQuery" -- ++
  --addJS "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/js/bootstrap.min.js" "/static/js/bootstrap.min.js" "(typeof $().modal != 'function')"

citHtml :: P.Citation -> Html
citHtml cit =
  H.span $ do
    H.i $ do
      toHtml $ (maybe "" (`T.append` ", ") $ cit^.P.citationJournal)
    H.b $ do
      toHtml $ maybe "" (`T.append` ", ") $ cit^.P.citationVolume
    toHtml $ (fromMaybe "" $ cit^.P.citationPageFrom)
    toHtml $ (maybe "" (T.append " - ") $ cit^.P.citationPageTo)
    toHtml $ maybe "" (\y -> " ("++show y ++")") $ cit^.P.citationYear
