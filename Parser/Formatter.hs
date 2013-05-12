{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Parser.Formatter (
  format
  , Format (..)
) where

-- import Parser.PaperReader
-- import System.Environment (getArgs)
import Data.List
-- import Data.Maybe (isJust)
import Control.Monad
-- import Control.Applicative
import Parser.Import hiding (Url,toHtml)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
-- import qualified Data.Text.IO as TIO
-- import Data.Aeson.Encode

import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy.Encoding (encodeUtf8)

-- import Settings
import Control.Lens
import Data.Tree

data Format = FormatA | FormatB deriving (Eq,Show,Ord)


format :: Format -> Paper -> LB.ByteString
format FormatA paper = encodeUtf8 $ renderHtml $ formatA paper
format FormatB paper = encodeUtf8 $ renderHtml $ formatB paper
-- format _ _ = error "Not supported format."


formatA :: Paper -> Html
formatA paper = docTypeHtml $ do
  H.head $ do
    H.title "Format A"
    forM_ cssPathsA $ \path -> do
      putCss path
  H.body $ do
    preEscapedToHtml $ maybe "" showMainText $ paper^.paperMainHtml

cssBasePath,jsBasePath :: String
cssBasePath = "http://localhost:3000/static/css/"
jsBasePath = "http://localhost:3000/static/js/"

cssPathsA, cssPathsB :: [String]
cssPathsA = map (cssBasePath ++) ["format_a.css"]
cssPathsB = map (cssBasePath ++) ["format_b.css","bootstrap.min.css"]

commonJS :: [String]
commonJS = ["http://code.jquery.com/jquery-1.9.1.min.js",
              jsBasePath++"bootstrap.min.js"]
jsPathsB :: [String]
jsPathsB = commonJS ++ map (jsBasePath ++) ["format-b-ui.js"]

putCss :: forall a. ToValue a => a -> Html
putCss url = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue url)
putJS :: String -> Markup
putJS url = H.script ! src (toValue url) $ H.toHtml (""::String)

emptyh :: Markup
emptyh = H.toHtml (""::String)

divi,divc :: String -> Html -> Markup
divi s = H.div ! A.id (toValue s)
divc s = H.div ! class_ (toValue s)

formatB :: Paper -> Html
formatB paper = docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ fromMaybe "(No title)" $ paper^.paperCitation^.citationTitle
    forM_ cssPathsB $ \path -> do
      putCss path
    preEscapedToHtml $ ("<meta name='viewport' content='width=device-width, initial-scale=1.0'>" :: String)
  H.body ! class_ "blacktext" $ do
    menuBar paper
    header $ do
      divi "topbox" $ do
        divc "right" $ emptyh
    divi "outerbox" $ do
      divi "leftbox" $ do
        divi "leftinner" $ do
          divi "titlediv" $ do
            maybe emptyh (\t -> H.span $ H.toHtml t) $ cit^.citationType
            h1 $ preEscapedToHtml $ fromMaybe "(No title)" $ cit^.citationTitle
            citationHtml cit
          divi "abstract" $ do
            preEscapedToHtml $ fromMaybe "" $ paper^.paperAbstract
          maybe emptyh renderSectionInfo $ paper^.paperSections
          divi "maintext" $ do
            preEscapedToHtml $ maybe "" showMainText $ paper^.paperMainHtml
      divi "rightbox" $ do
        referencesHtml (paper^.paperReferences)
    forM_ jsPathsB $ \path -> do
      putJS path
  where
    cit = paper^.paperCitation

toHtml' :: String -> Html
toHtml' = toHtml

preEscapedToHtml' :: String -> Html
preEscapedToHtml' = preEscapedToHtml

citationHtml :: Citation -> Html
citationHtml cit = do
  H.p ! A.id "citation" $ do
    toHtml $ T.intercalate ", " (cit^.citationAuthors)
    br
    H.i $ toHtml $ fromMaybe "" $ cit^.citationJournal
    toHtml' ", "
    H.b $ toHtml $ fromMaybe "" $ cit^.citationVolume
    toHtml' ", "
    toHtml $ fromMaybe "" $ cit^.citationPageFrom
    preEscapedToHtml ("&dash;" :: String)
    toHtml $ fromMaybe "" $ cit^.citationPageTo
    case cit^.citationYear of
      Just year -> do
        preEscapedToHtml' "&nbsp;"
        toHtml $ "(" ++ show year ++ ")"
      Nothing -> emptyh

referencesHtml :: [Reference] -> Html
referencesHtml refs = do
  H.h3 ! A.id "references" $ toHtml' "References"
  H.ul ! class_ "references" $ do
    forM_ refs $ \ref -> do
      H.li ! A.id (toValue (_referenceRefId ref)) $ do
        H.span ! class_ "refname" $ do
          toHtml $ _referenceRefName ref
        toHtml $ fromMaybe (mkCitText ref) (_referenceCitText ref)

renderSectionInfo :: SectionInfo -> Html
renderSectionInfo (SectionInfo (Node _ forest)) = do
  divi "sectioninfo" $ do
    ul ! A.id "section_list" $ do
      forM_ forest $ \(Node (Section str _) _) -> do
        li $ H.toHtml str


menuBar paper =
  let
    cit = paper^.paperCitation
  in do
  H.div ! class_ "navbar" $ do
    divc "navbar-inner" $ do
      H.a ! class_ "brand" ! A.href "#" $ toHtml' "Papers"
      -- ul ! class_ "nav"
      H.span ! class_ "navbar-text" ! A.id "headerCitation" $ do
        H.i $ toHtml $ fromMaybe "" $ cit^.citationJournal
        toHtml' ", "
        H.b $ toHtml $ fromMaybe "" $ cit^.citationVolume
        preEscapedToHtml $ T.concat
                  [", ", fromMaybe "" $ cit^.citationPageFrom,
                    ("&dash;"::T.Text), fromMaybe "" $ cit^.citationPageTo]

