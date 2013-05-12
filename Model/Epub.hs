{-# LANGUAGE OverloadedStrings #-}
module Model.Epub (
  epubFromPaper
) where

import Import hiding (Paper(..),Citation(..),Figure(..))

import System.Process
import System.Directory
import Control.Exception (try,IOException)
import Data.Time

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Char8
import Data.String
import qualified Data.Text as T
-- import Data.Tree
import Data.Text.Encoding

-- import Data.Maybe
-- import Control.Monad (forM_,mapM_)
import Control.Lens

import Text.Blaze
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

-- import Handler.Utils (getResourceId)
-- import Model (Paper)
import Model.PaperReader
import Model.PaperP (renderStructured)
import Parser.Paper as P hiding (Paper,Url)
import qualified Parser.Paper as P

import Data.FileEmbed

epubFromPaper :: PaperId -> P.Paper -> Handler FilePath
epubFromPaper pid paper = do
  let
    strpid = T.unpack (toPathPiece pid)
    dir = epubSourceFolder ++ strpid
    epubpath = epubSourceFolder ++ strpid ++ ".epub"
  liftIO $ system $ "mkdir " ++ dir
  liftIO $ setCurrentDirectory dir
  liftIO $ system $ "mkdir META-INF"
  liftIO $ system $ "mkdir OEBPS"
  let containerStr
                  = BS.concat ["<?xml version='1.0'?>"
                             ,"<container version='1.0' xmlns='urn:oasis:names:tc:opendocument:xmlns:container'>"
                             ,"<rootfiles>"
                             ,"<rootfile full-path='OEBPS/container.opf' media-type='application/oebps-package+xml' />"
                            , "</rootfiles> </container>"]
  liftIO $ BS.writeFile (dir ++ "/META-INF/container.xml") containerStr
  liftIO $ mkOpf pid paper dir
  mkPaper pid paper dir
  figPaths <- liftIO $ mapM (\f -> mkFig f dir) (paper^.P.paperFigures)
  liftIO $ mkNav pid paper dir figPaths
  liftIO $ BS.writeFile (dir++"/mimetype") "application/epub+zip"
  liftIO $ system $ "zip -0 -X "++"../"++strpid++".epub ./mimetype"
  liftIO $ system $ "zip -r ../"++strpid++".epub ./* -x ./mimetype"
  -- system $ "rm -r '" ++ dir ++ "'" 
  return epubpath 

mkNav :: PaperId -> P.Paper -> FilePath -> [(String, String)] -> IO ()
mkNav pid paper dir figPaths = do
  let
    mkLi (name,path) = BS.concat ["<li><h2>",fromString name,"</h2><a epub:type='loi' href='",fromString path,"'>",fromString name,"</a></li>"]
    lis = [ "<li><a epub:type='bodymatter' href='paper1.html'>Main</a></li>" ] ++
            Import.map mkLi figPaths
    navhtml = BS.concat $
      ["<nav epub:type='toc' id='toc'>"
      , "<h2>Contents</h2>"
      , "<ol>"] ++ lis ++
      ["</ol></nav>"]
    bs = BS.concat ["<?xml version='1.0' encoding='UTF-8'?>"
           , "<html xmlns='http://www.w3.org/1999/xhtml' xmlns:epub='http://www.idpf.org/2011/epub' xml:lang='en'>"
           , "<head><meta charset='UTF-8'/><title></title>" 
           , "</head><body>"
           , navhtml
           , "</body> </html>"]
  BS.writeFile (dir++"/OEBPS/nav.html") bs

mkPaper :: PaperId -> PaperP -> FilePath -> Handler ()
mkPaper pid pp dir = do
  let
    mabstract = pp^.P.paperAbstract
    mainHtml = maybe "" renderStructured $ pp^.P.paperMainHtml
  let cit = pp^.P.paperCitation
  let html = do
                H.head $ do
                  H.title $ toHtml $ fromMaybe "(No title)" (pp^.P.paperCitation^.P.citationTitle)
                  H.style ! A.type_ "text/css" $ preEscapedToHtml cssText
                H.body $ do
                  H.div ! A.id "titlediv" $ do
                    maybe emptyh (\t -> H.span $ H.toHtml t) $ cit^.citationType
                    h1 $ preEscapedToHtml $ fromMaybe "(No title)" $ cit^.citationTitle
                    citationHtml cit
                  case mabstract of
                    Just abs -> do
                      H.div ! A.id "abstract" $ do
                        preEscapedToHtml abs
                    Nothing ->
                      emptyh
                  H.div mainHtml
  let bs = BL.concat ["<?xml version='1.0' encoding='UTF-8'?><html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en'>",renderHtml html]
  liftIO $ BL.writeFile (dir++"/OEBPS/paper1.html") bs

emptyh :: Markup
emptyh = H.toHtml (""::String)


preEscapedToHtml' :: String -> Html
preEscapedToHtml' = preEscapedToHtml

toHtml' :: String -> Html
toHtml' = H.toHtml

citationHtml :: P.Citation -> Html
citationHtml cit = do
  H.p ! A.id "citation" $ do
    H.toHtml $ T.intercalate ", " (cit^.citationAuthors)
    br
    H.i $ H.toHtml $ fromMaybe "" $ cit^.citationJournal
    toHtml' ", "
    H.b $ H.toHtml $ fromMaybe "" $ cit^.citationVolume
    toHtml' ", "
    H.toHtml $ fromMaybe "" $ cit^.citationPageFrom
    preEscapedToHtml' "&dash;"
    H.toHtml $ fromMaybe "" $ cit^.citationPageTo
    case cit^.citationYear of
      Just year -> do
        preEscapedToHtml' "&nbsp;"
        H.toHtml $ "(" ++ show year ++ ")"
      Nothing -> emptyh

mkFig :: P.Figure -> FilePath -> IO (String,String)
mkFig fig dir = do
  let
    num = T.unpack $ fig^.P.figId
    name = T.unpack $ fig^.P.figName
    url = fig^.figImg
    figfile = "fig_"++num++".png" -- ++ imgExt (getImgType url)  Stub: currently all images are png.
    imgPath = resourceRootFolder ++ mkFileName (T.unpack url)
    html = do
               head $ do
                 H.title $ toHtml name
               body $ do
                 H.h1 $ toHtml name
                 H.div $ do
                   H.div $ do
                     H.img ! src (fromString figfile)
                   H.div $ preEscapedToHtml (fig^.figAnnot)
    bs = "<?xml version='1.0' encoding='UTF-8'?><html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en'>"
            `BL.append` renderHtml html
    htmlfile = "fig_"++num++".html"
  putStrLn $ T.unpack url
  _ <- try (copyFile imgPath (dir ++ "/OEBPS/" ++ figfile)) :: IO (Either IOException ())
  BL.writeFile (dir++"/OEBPS/"++htmlfile) bs
  return (name,htmlfile)
  
mkOpf :: PaperId -> PaperP -> FilePath -> IO ()
mkOpf pid paper dir = do
  let
    cit = paper^.paperCitation
    title = encodeUtf8 $ fromMaybe "(No title)" $ cit^.citationTitle
    authors = cit^.citationAuthors
    pub = encodeUtf8 $ fromMaybe "N/A" $ cit^.citationPublisher
  ts <- fmap show getCurrentTime
  let
    time = fromString $ take 19 ts
    authors_str = BS.concat $ Import.map (\s -> encodeUtf8 $ T.concat ["<dc:creator>",s,"</dc:creator>"]) authors
    fignums = Import.map (T.unpack . _figId) $ paper^.paperFigures
    fightmls = BS.concat $ Import.map figitem fignums
    figtypes = Import.map (getImgType . _figImg) $ paper^.paperFigures
    figimgs = BS.concat $ Import.map figimg $ zip fignums figtypes
    figrefs = fromString $ Import.concatMap (\n -> "<itemref idref='fightml_"++n++"'/>") fignums
    bs = BS.concat ["<?xml version='1.0' encoding='UTF-8'?>",
             "<package version='3.0' xmlns='http://www.idpf.org/2007/opf' unique-identifier='db-id'> <metadata xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:opf='http://www.idpf.org/2007/opf'> <dc:title>",title,"</dc:title>",
             authors_str,
             "<dc:language>ja</dc:language> <dc:rights>Public Domain</dc:rights> <dc:publisher>",pub,"</dc:publisher> <dc:identifier id='db-id'>", (fromString . T.unpack . toPathPiece) pid,"</dc:identifier><meta property='dcterms:modified'>",time,"</meta> </metadata>",
             "<manifest><item id='nav.xhtml' href='nav.html' properties='nav' media-type='application/xhtml+xml'/>",
             "<item id='html1' href='paper1.html' media-type='application/xhtml+xml' />",
             fightmls,
             figimgs,
             "</manifest>",
             "<spine page-progression-direction='ltr'><itemref idref='html1' />",
             figrefs,
             "</spine> </package>"]
  BS.writeFile (dir++"/OEBPS/container.opf") bs


figitem :: String -> BS.ByteString
figitem num = fromString $ "<item id='fightml_"++num++"' href='fig_" ++ num ++".html' media-type='application/xhtml+xml' />"

getImgType :: Url -> ImgType
getImgType u = ImgPng {-  --stub: currenly all png
  | ".jpg" `T.isSuffixOf` u = ImgJpeg
  | ".jpeg" `T.isSuffixOf` u = ImgJpeg
  | ".png" `T.isSuffixOf` u = ImgPng
  | ".gif" `T.isSuffixOf` u = ImgGif
  | otherwise = ImgUnknown -}

data ImgType = ImgGif | ImgJpeg | ImgPng | ImgUnknown

figimg :: (String,ImgType) -> BS.ByteString
figimg (num,itype) = fromString $ "<item id='fig_"++num++"' href='fig_" ++ num ++"." ++imgExt itype++"' media-type='"++imgMime itype++"' />"
  where

imgExt :: ImgType -> String
imgExt ImgGif = "gif"
imgExt ImgJpeg = "jpeg"
imgExt ImgPng = "png"
imgExt ImgUnknown = "png"  -- stub: all images are now png.
-- imgExt ImgUnknown = "unknown"

-- Stub
imgMime ImgGif = "image/gif"
imgMime ImgJpeg = "image/jpeg"
imgMime ImgPng = "image/png"
imgMime ImgUnknown = "image/png"


cssText :: Text
cssText = (T.strip . decodeUtf8) $(embedFile "static/css/epub.css")
