{-# LANGUAGE TemplateHaskell,TypeFamilies,EmptyDataDecls,DeriveDataTypeable,GADTs #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax
import Data.Typeable (Typeable)

import Data.Maybe
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Time.Clock
import qualified Parser.Paper as P
import Parser.JSON

import Model.Defs
import Data.Data
    
{-
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

import Data.Aeson.TH

import qualified Parser.Paper as P
-- import Database.Persist.Store

-}


--  sqlType _ = SqlInt32
--  isNullable _ = False

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoBackend))
                        { mpsGeneric = False
                        }
 in share [mkPersist mongoSettings]
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

{-
-- default values
emptyCitation :: Citation
emptyCitation = Citation Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing
-}
emptyMisc :: ByteString
emptyMisc = ""

emptyRefs :: [Reference]
emptyRefs = []

defaultTime :: UTCTime
defaultTime = read "1970-01-01 01:01:01 +0000"

{-
emptyPaper :: Paper
emptyPaper = Paper
               "" ""   -- doi and url
               "" Nothing Nothing   -- html, abs, main
               emptyCitation emptyRefs [] []   -- cit ref fig res
               Nothing [] Nothing emptyMisc  -- toc tags note misc 
               Nothing P.SUndecidable   --- parserInfo, supportLevel
               (ResourceAvailability False False False False False False)
               Nothing defaultTime    -- email, time
-}
