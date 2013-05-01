module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

import Data.Aeson.TH

import Data.Time.Clock
import qualified Parser.Paper as P
import Database.Persist.Store

type Url = Text

data HistoryAction = HACreate | HAView | HARemove deriving (Show,Eq,Enum)
data LocalCopyStatus = LocalAvailable | NotYet | Failed | Unknown deriving (Show,Read,Eq)

$(deriveJSON id ''HistoryAction)
$(deriveJSON id ''LocalCopyStatus)

instance PersistField P.SupportLevel where
  toPersistValue P.SFullText = PersistText "fulltext"
  toPersistValue P.SAbstract = PersistText "abstract"
  toPersistValue P.SCitation = PersistText "citation"
  toPersistValue P.SUndecidable = PersistText "unknown"

  fromPersistValue (PersistText "fulltext") = Right P.SFullText
  fromPersistValue (PersistText "abstract") = Right P.SAbstract
  fromPersistValue (PersistText "citation") = Right P.SCitation
  fromPersistValue (PersistText "unknown") = Right P.SUndecidable
  fromPersistValue _ = Left "Not supported"

  sqlType _ = SqlString
  isNullable _ = False


instance PersistField HistoryAction where
  toPersistValue HACreate = PersistInt64 1
  toPersistValue HAView = PersistInt64 2
  toPersistValue HARemove = PersistInt64 3

  fromPersistValue (PersistInt64 1) = Right HACreate
  fromPersistValue (PersistInt64 2) = Right HAView
  fromPersistValue (PersistInt64 3) = Right HARemove
  fromPersistValue _ = Left "Not supported"

  sqlType _ = SqlInt32
  isNullable _ = False


instance PersistField LocalCopyStatus where
  toPersistValue LocalAvailable = PersistInt64 0
  toPersistValue NotYet = PersistInt64 1
  toPersistValue Failed = PersistInt64 2
  toPersistValue Unknown = PersistInt64 3

  fromPersistValue (PersistInt64 0) = Right LocalAvailable
  fromPersistValue (PersistInt64 1) = Right NotYet
  fromPersistValue (PersistInt64 2) = Right Failed
  fromPersistValue _ = Right Unknown

  sqlType _ = SqlInt32
  isNullable _ = False

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoBackend))
                        { mpsGeneric = False
                        }
 in share [mkPersist mongoSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


-- This is a general summary function. cf. summaryEx in Handler.PaperListW2UI
-- This just returns a pair, so that you can add or delete elements before applying object.
-- Caution: you need to apply object to get a dictionary type, otherwise, this will generate an array.
paperSummary :: PaperId -> Paper -> [(Text,Value)] 
paperSummary pid p
  = ["id" .= toPathPiece pid, "doi" .= paperDoi p, "url" .= paperUrl p, "title" .= citationTitle c,
            "tags" .= (paperTags p), "note" .= paperNote p , "citation" .= toJSON c 
            , "cittxt" .= mkCitTxt c]
  where
    c = paperCitation p
    mkCitTxt c = T.concat
                  ["<i>"
                  , fromMaybe "" (citationJournal c)
                  , "</i>, "
                  , maybe "" (\v -> T.concat["<b>",v,"</b>"]) (citationVolume c)
                  , ", "
                  , fromMaybe "" (citationPageFrom c)
                  , maybe "" (\to -> "-" `T.append` to) (citationPageTo c)
                  , maybe "" (\y -> T.pack $ " (" ++ show y ++ ")") (citationYear c)
                  ]

paperSummary' :: Entity Paper -> Value
paperSummary' (Entity pid p) = object $ paperSummary pid p


-- default values
emptyCitation :: Citation
emptyCitation = Citation Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing

emptyMisc :: ByteString
emptyMisc = ""

emptyRefs :: [Reference]
emptyRefs = []

emptyPaper :: Paper
emptyPaper = Paper
               "" ""   -- doi and url
               "" Nothing Nothing   -- html, abs, main
               emptyCitation emptyRefs [] []   -- cit ref fig res
               Nothing [] Nothing emptyMisc  -- toc tags note misc 
               Nothing P.SUndecidable   --- parserInfo, supportLevel
               Nothing     -- email

