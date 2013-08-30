{-# LANGUAGE QuasiQuotes,TemplateHaskell #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.

{-
import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.MongoDB (MongoConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import Text.Hamlet

import Yesod.Dispatch (toPathPiece)
import Model (PaperId)

-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.MongoDB (MongoConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import Text.Hamlet

import Data.Digest.Pure.SHA (sha256,showDigest)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as BC


-- | Which Persistent backend this site is using.
type PersistConf = MongoConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"


appRootFolder, webmasterEmail, analyticsCode, hostname :: String

-- I need to double check but appRootFolder should not have a space
-- since this is passed as arg for command line program.

appRootFolder = (T.unpack . T.strip . decodeUtf8) $(embedFile "config/approotfolder")
hostname = (T.unpack . T.strip . decodeUtf8) $(embedFile "config/hostname")
webmasterEmail = (T.unpack . T.strip . decodeUtf8) $(embedFile "config/webmaster_email")
analyticsCode = (T.unpack . T.strip . decodeUtf8) $(embedFile "config/analytics_code")

-- [mongoUser,mongoPassword] = (T.splitOn "/" . T.strip . decodeUtf8) $(embedFile "config/mongo_login")

genericReaderTargets :: [Text]
genericReaderTargets = (filter (not . T.null) . map T.strip . T.lines . decodeUtf8) $(embedFile "config/generic_reader_targets")

bookmarklet :: Text
bookmarklet = T.concat ["javascript:document.getElementsByTagName('body')%5B0%5D.appendChild(document.createElement('script')).setAttribute('src','http://",T.pack hostname,"/static/js/bookmarklet.js');"]

epubTemplateFolder :: String
epubSourceFolder :: String
htmlFolder :: String
imageCachePath :: Text -> String

epubTemplateFolder = appRootFolder ++ "temp/epub_template/"
epubSourceFolder = appRootFolder ++ "temp/epub_source/"

htmlFolder = appRootFolder ++ "data/paper/"

cacheFileBaseName :: Text -> Text -> String
cacheFileBaseName email url = htmlFolder ++ (mkFileName $ T.unpack . T.concat $ [email,"::",url])


imageCachePath url = appRootFolder ++ "data/image/" ++ (mkFileName $ T.unpack url)

-- |Make a file name from url. This is used everywhere.
mkFileName :: String -> String
mkFileName = filter (/= ' ') . showDigest . sha256 . BL.fromChunks . (:[]) . BC.pack


resourceRootUrl :: String
resourceRootUrl = "/resource/"

resourceRootFolder = appRootFolder ++ "data/image/"


-- JS and CSS URLs
data Asset = BootStrap | JQuery | JQueryR | W2UI | BootStrapC | JQueryC | W2UIC | JQMobile | JQMobileC |
                Underscore
asset :: Asset -> Text
asset BootStrap = "/static/js/lib/bootstrap.min.js"
asset JQueryR = "http://code.jquery.com/jquery-1.9.1.min.js"
asset JQuery = "/static/js/lib/jquery.min.js"
asset W2UI = "/static/js/lib/w2ui-1.1.min.js"
asset BootStrapC = "/static/css/lib/bootstrap-combined.min.css"
asset JQueryC = "/static/css/lib/jquery.min.css"
asset W2UIC = "/static/css/lib/w2ui-1.1.min.css"
asset JQMobile = "http://code.jquery.com/mobile/1.3.1/jquery.mobile-1.3.1.min.js"
asset JQMobileC = "http://code.jquery.com/mobile/1.3.1/jquery.mobile-1.3.1.min.css"
asset Underscore = "/static/js/lib/underscore-min.js"
-- asset _ = error "Not found"
