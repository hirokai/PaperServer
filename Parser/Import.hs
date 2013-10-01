{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}

module Parser.Import (
module Parser.Import
, module Parser.Paper
, module Parser.PaperReaderTypes
-- , module Parser.Lens
, module Text.XML.Selector
, module Text.XML.Selector.TH
, module Text.XML.Scraping
, module B
, module LB
, module Data.Maybe
, module Safe
, module Debug.Trace
, module Data.Monoid
, module Prelude
, module Data.Default
) where

import Prelude hiding (head,tail)
import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text as Parser.Import (Text)

import Text.XML.Selector
import Text.XML.Selector.TH
import Text.XML.Scraping

import Safe

import Parser.Paper
import Parser.PaperReaderTypes
-- import Parser.Lens
import Debug.Trace
import Data.Monoid
import Data.Default


{-
-- This is for checking the use of head.
head :: [a] -> a
head (x:_) = x
head _ = error "Don't use head on an empty list."

tail :: [a] -> [a]
tail (_:xs) = xs
tail _ = error "Don't use head on an empty list." -}
