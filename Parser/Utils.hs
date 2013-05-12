{-# LANGUAGE OverloadedStrings,ImplicitPrelude #-}

module Parser.Utils
  (
      head'
    , tail'
    , inner
    , render
    , takeb
    , dropm
    , takemt
    , dropmt
    , eitherMaybe
    , takeCurUntil
    , mimeFromUrl
    , (<||>) 
  ) where

-- import Import

import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML.Selector
import Text.XML.Scraping as S

-- import qualified Data.ByteString.Lazy as BL
-- import Data.Digest.Pure.SHA (sha256,showDigest)
-- import Data.ByteString.Char8 (pack)

-- import Data.Maybe
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

takemt :: Int -> T.Text -> Maybe T.Text
takemt n xs | T.length xs >= n = Just $ T.take n xs
           | otherwise = Nothing

dropmt :: Int -> T.Text -> Maybe T.Text
dropmt n xs | T.length xs >= n = Just $ T.drop n xs
           | otherwise = Nothing

inner :: [Cursor] -> Maybe T.Text
inner = maybeText . TL.toStrict . S.innerHtml

render :: [Node] -> Maybe T.Text
render = maybeText . TL.toStrict . renderNodes

-- drop' :: Int -> T.Text -> T.Text
-- drop' n s = if length s >= n then T.drop n s else ""


eitherMaybe :: Either a b -> Maybe b
eitherMaybe (Left _) = Nothing
eitherMaybe (Right x) = Just x 


decodeNonLatin :: T.Text -> T.Text
decodeNonLatin cs = foldl1 (.) (map f [("|[eacute]|","&#201;")]) $ cs
  where
    f :: (T.Text,T.Text) -> (T.Text -> T.Text)
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
