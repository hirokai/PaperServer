{-# LANGUAGE TemplateHaskell #-}

module Parser.JSON where

import Prelude
import Data.Char (toLower)
import Data.Aeson.TH
import Parser.Paper

import Data.Tree

$(deriveJSON id ''PaperMainText)
$(deriveJSON id ''Figure)

$(deriveJSON (\s -> (toLower (s !! 9)):(drop 10 s)) ''Citation)
$(deriveJSON (\s -> (toLower (s !! 9)):(drop 10 s)) ''Resource)
$(deriveJSON (\s -> (toLower (s !! 4)):(drop 5 s)) ''Reference)
$(deriveJSON (\s -> (toLower (s !! 4)):(drop 5 s)) ''Section)
$(deriveJSON id ''Tree)
$(deriveJSON (\s -> (toLower (s !! 4)):(drop 5 s)) ''SectionInfo)
$(deriveJSON (\s -> (toLower (s !! 6)):(drop 7 s)) ''Paper)
$(deriveJSON (\s -> (map toLower (drop 1 s))) ''SupportLevel)
