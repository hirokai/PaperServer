{-# LANGUAGE QuasiQuotes #-}

module Model.RIS where

import Import
import qualified Data.Text as T
import qualified Data.Map as M

import Text.Regex.PCRE.Rex

import Safe
import Control.Applicative

import Data.Default

data RISInfo = RISInfo {
  risTitle :: Maybe Text,
  risJournal :: Maybe Text,
  risAuthors :: Maybe [Text],
  risVolume :: Maybe Text,
  risYear :: Maybe Int,
  risPageFrom :: Maybe Text,
  risPageTo :: Maybe Text
} deriving (Eq,Show)

instance Default RISInfo where
  def = RISInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parseRIS :: Text -> Maybe RISInfo
parseRIS txt = Just . f . fromList' . map (\(a,b) -> (T.pack a,T.pack b)) . catMaybes . map p . lines . T.unpack $ txt
  where
    p line = [rex|^(?{}\w+)\s+-\s+(?{}.+)$|] line
    f m = RISInfo (M.lookup "T1" m >>= headMay)
                  (M.lookup "JO" m >>= headMay)
                  (M.lookup "A1" m)
                  (M.lookup "VL" m >>= headMay)
                  (M.lookup "Y1" m >>= headMay >>= (readMay . T.unpack))
                  (M.lookup "SP" m >>= headMay)
                  (M.lookup "EP" m >>= headMay)

fromList' :: (Ord a) => [(a,b)] -> M.Map a [b]
fromList' ns = g ns (M.empty)
  where
    g :: (Ord a) => [(a,b)] -> M.Map a [b] -> M.Map a [b]
    g [] m = m
    g ((k,v):ns) m = g ns (M.insert k vv m)
      where
        vv = case M.lookup k m of
                Just vs -> (v:vs)
                Nothing -> [v]