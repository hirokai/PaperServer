module Model.PaperReaderTypes where

import Import

import Text.XML
import qualified Data.Text as T (pack,unpack,concat,Text,replace,append,intercalate,splitOn,head)
import Data.List as L
import qualified Data.Map as M

-- TODO: Differentiate title in Paper and title in Citation
-- title in Citation should be plain text
-- title in Paper should be real title including superscript, greek letter, by html


parseNothing _ _ = Nothing

absolutePath :: T.Text -> T.Text -> T.Text
absolutePath url path
  | T.head path == '/' = (T.intercalate "/" $ take 3 (T.splitOn "/" url)) `T.append` path
  | otherwise = (T.intercalate "/" $ init (T.splitOn "/" url)) `T.append` "/" `T.append` path


doi2url :: String -> String
doi2url doi = "http://dx.doi.org/" ++ doi

-- emptyMisc = B.concat $ LB.toChunks $ MP.pack (M.fromList [] :: M.Map Int Int)

nodeToText :: Node -> Text
nodeToText (NodeElement e)
    = T.concat $ ["<", name, " ", attr, ">"]
   ++ map nodeToText (elementNodes e)
   ++ (if isSingle name then [] else ["</", name,">"])
     where
        name = nameLocalName $ elementName e
        isSingle n = n `elem` [ "area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
        attr = T.pack $ intercalate " " $ map f  (M.toList $ elementAttributes e)
          where f (k,v) = (T.unpack $ nameLocalName k) ++ "=\"" ++(T.unpack v) ++"\""
nodeToText (NodeContent t) = t
nodeToText (NodeInstruction _) = ""
nodeToText (NodeComment _) = ""


decodeUrl :: T.Text -> T.Text
decodeUrl s = T.replace "%2F" "/" s

