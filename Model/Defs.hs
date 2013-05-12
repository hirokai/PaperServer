module Model.Defs where

import Prelude
import Database.Persist.Class
import Database.Persist.Sql
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T
import Data.Aeson.TH
import Data.Char
import qualified Parser.Paper as P
import Data.Typeable
type Url = Text

data HistoryAction = HACreate | HAView | HARemove | HAVisitOriginal deriving (Show,Eq,Enum,Typeable)
data LocalCopyStatus = LocalAvailable | NotYet | Failed | Unknown deriving (Show,Read,Eq,Typeable)

data ResourceAvailability = ResourceAvailability {
  raCitation :: Bool
  , raAbstract :: Bool
  , raFulltext :: Bool
  , raFigures :: Bool
  , raReferences :: Bool 
  , raToc :: Bool 
} deriving (Show,Eq)

instance PersistField ResourceAvailability where
  toPersistValue (ResourceAvailability cit abs full fig ref toc)
    = PersistText $ T.intercalate "," $ catMaybes
       [if cit then Just "cit" else Nothing
       , if abs then Just "abs" else Nothing
       , if full then Just "full" else Nothing
       , if fig then Just "fig" else Nothing
       , if ref then Just "ref" else Nothing
       , if toc then Just "toc" else Nothing]
  fromPersistValue (PersistText txt)
    = Right $ f (T.splitOn "," txt)
    where
      f ts = ResourceAvailability
               ("cit" `elem` ts) 
               ("abs" `elem` ts) 
               ("full" `elem` ts) 
               ("fig" `elem` ts) 
               ("ref" `elem` ts) 
               ("toc" `elem` ts) 

instance PersistFieldSql ResourceAvailability where
  sqlType _ = SqlString


$(deriveJSON id ''HistoryAction)
$(deriveJSON id ''LocalCopyStatus)

$(deriveJSON (\s -> (toLower (s !! 2)):(drop 3 s)) ''ResourceAvailability)

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

instance PersistFieldSql P.SupportLevel where
  sqlType _ = SqlString


instance PersistField HistoryAction where
  toPersistValue HACreate = PersistInt64 1
  toPersistValue HAView = PersistInt64 2
  toPersistValue HARemove = PersistInt64 3
  toPersistValue HAVisitOriginal = PersistInt64 4

  fromPersistValue (PersistInt64 1) = Right HACreate
  fromPersistValue (PersistInt64 2) = Right HAView
  fromPersistValue (PersistInt64 3) = Right HARemove
  fromPersistValue (PersistInt64 4) = Right HAVisitOriginal
  fromPersistValue _ = Left "Not supported"

instance PersistFieldSql HistoryAction where
  sqlType _ = SqlInt32


instance PersistField LocalCopyStatus where
  toPersistValue LocalAvailable = PersistInt64 0
  toPersistValue NotYet = PersistInt64 1
  toPersistValue Failed = PersistInt64 2
  toPersistValue Unknown = PersistInt64 3

  fromPersistValue (PersistInt64 0) = Right LocalAvailable
  fromPersistValue (PersistInt64 1) = Right NotYet
  fromPersistValue (PersistInt64 2) = Right Failed
  fromPersistValue _ = Right Unknown

instance PersistFieldSql LocalCopyStatus where
  sqlType _ = SqlInt32

