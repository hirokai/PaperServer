{-# LANGUAGE TemplateHaskell #-}

module Parser.Lens where

import Prelude
import Control.Lens
import Parser.Paper
import Data.Text (Text)
import Data.Maybe
import Data.ByteString (ByteString)

makeLenses ''Paper
makeLenses ''Citation
makeLenses ''Reference
makeLenses ''Resource
makeLenses ''Section
makeLenses ''Figure


-- Synonyms for Paper

pdoi,purl,html :: Lens Paper Paper Text Text
pdoi = paperDoi
purl = paperUrl
html = paperHtml

abstract :: Lens Paper Paper (Maybe Text) (Maybe Text)
abstract = paperAbstract

mainHtml :: Lens Paper Paper (Maybe PaperMainText) (Maybe PaperMainText)
mainHtml = paperMainHtml

figures :: Lens Paper Paper [Figure] [Figure]
figures = paperFigures

references :: Lens Paper Paper [Reference] [Reference]
references = paperReferences

resources :: Lens Paper Paper [Resource] [Resource]
resources = paperResources

citation :: Lens Paper Paper Citation Citation
citation = paperCitation

toc,note :: Lens Paper Paper (Maybe Text) (Maybe Text) 
toc = paperToc
note = paperNote

tags :: Lens Paper Paper [Text] [Text]
tags = paperTags

misc :: Lens Paper Paper ByteString ByteString
misc = paperMisc

ptype :: Lens Citation Citation (Maybe Text) (Maybe Text) 
ptype = citationType

parserInfo :: Lens Paper Paper (Maybe Text) (Maybe Text)
parserInfo = paperParserInfo

sections :: Lens Paper Paper (Maybe SectionInfo) (Maybe SectionInfo)
sections = paperSections

supportLevel :: Lens Paper Paper SupportLevel SupportLevel
supportLevel = paperSupportLevel

-- Synonyms for Citation

publisher, journal, title, doi, url, pageFrom, pageTo, volume
	:: Lens Citation Citation (Maybe Text) (Maybe Text)
year :: Lens Citation Citation (Maybe Int) (Maybe Int)

publisher = citationPublisher
journal = citationJournal
title = citationTitle
doi = citationDoi
url = citationUrl
pageFrom = citationPageFrom
pageTo = citationPageTo
year = citationYear
volume = citationVolume

authors :: Lens Citation Citation [Text] [Text]
authors = citationAuthors
