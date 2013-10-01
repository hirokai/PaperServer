{-# LANGUAGE OverloadedStrings,ImplicitPrelude #-}

module Parser.Utils
  (
      head'
    , tail'
    , inner
    , innert
    , render
    , takeb
    , dropm
    , takemt
    , dropmt
    , eitherMaybe
    , takeCurUntil
    , mimeFromUrl
    , (<||>) 
    , samePaper
    , mkCitText
  ) where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML.Selector
import Text.XML.Scraping as S
import Data.Traversable

import Control.Lens
import Parser.Paper
import Parser.Lens
import Data.Maybe

import Safe

head' :: [a] -> [a]
head' [] = []
head' (x:_) = [x]

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

-- | take bottom.
takeb :: Int -> [a] -> [a]
takeb n = reverse . take n . reverse

takem :: Int -> [a] -> Maybe [a]
takem n xs | length xs >= n = Just $ take n xs
           | otherwise = Nothing

dropm :: Int -> [a] -> Maybe [a]
dropm n xs | length xs >= n = Just $ drop n xs
           | otherwise = Nothing

takemt :: Int -> Text -> Maybe Text
takemt n xs | T.length xs >= n = Just $ T.take n xs
           | otherwise = Nothing

dropmt :: Int -> Text -> Maybe Text
dropmt n xs | T.length xs >= n = Just $ T.drop n xs
           | otherwise = Nothing

inner :: [Cursor] -> Maybe Text
inner = maybeText . TL.toStrict . S.innerHtml

innert :: [Cursor] -> Maybe Text
innert = maybeText . TL.toStrict . S.innerText

render :: [Node] -> Maybe Text
render = maybeText . TL.toStrict . toHtml

-- drop' :: Int -> Text -> Text
-- drop' n s = if length s >= n then T.drop n s else ""


eitherMaybe :: Either a b -> Maybe b
eitherMaybe (Left _) = Nothing
eitherMaybe (Right x) = Just x 


decodeNonLatin :: Text -> Text
decodeNonLatin cs = foldl1 (.) (map f [("|[eacute]|","&#201;")]) $ cs
  where
    f :: (Text,Text) -> (Text -> Text)
    f (from,to) = T.replace from to


takeCurUntil :: (Cursor -> Bool) -> Cursor -> [Cursor]
takeCurUntil pred cur = takeWhile (not . pred) ((orSelf following) cur)

mimeFromUrl :: Text -> Text
mimeFromUrl url =
  case lastMay (T.splitOn "." url) of
    Just "png" -> "image/png"
    Just "jpg" -> "image/jpeg"
    Just "jpeg" -> "image/png"
    Just "gif" -> "image/gif"
    _ -> "unknown"   -- Stub

infixl 3 <||>

(<||>) :: [a] -> [a] -> [a]
as <||> bs =
  if null as then
    bs
  else
    as


samePaper :: Citation -> Citation -> Bool
samePaper c1 c2 = (c1^.citationDoi) == (c2^.citationDoi) || (c1^.citationUrl) == (c2^.citationUrl)

--ToDo: Complete this.
mkCitText :: Reference -> Text
mkCitText ref
  = case _referenceCit ref of
      Just c -> (fromMaybe "" $ c^.citationJournal) `T.append` (maybe "" (", " `T.append`) $
                  fmap (T.pack . show) $ c^.citationYear)
      Nothing -> ""
