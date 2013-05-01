module Model.MsgPack where

--
-- MessagePack import/export
--

{-

import Data.MessagePack as MP


msgPackFromPaper :: Paper -> MP.Object
msgPackFromPaper p
    = toObject
      [("doi"::String,toObject (paperDoi p)),("url",toObject (paperUrl p)),
      ("html",toObject (paperHtml p)),("abstract",toObject (paperAbstract p)),
      ("mainHtml",toObject (paperMainHtml p)),("citation",msgPackFromCitation c),
      ("references",toObject $ map (map msgPackFromReference) rss),("figures",toObject $ map msgPackFromFigure fs),
      ("tocstr",toObject (paperToc p)),("misc",toObject (paperMisc p)),
      ("tags",toObject (paperTags p)),("note",toObject (paperNote p))]
    where
      c = paperCitation p
      fs = paperFigures p
      rss = paperReferences p


-- ToDo: This does not work correctly.
-- Do not import UserEmail even if there is any.
-- Stub: Fig, ref, cit, etc should be imported correctly.

paperFromMsgPack obj =
  case MP.tryFromObject obj :: Either String [(String,MP.Object)] of
    Right ts
      -> Paper <$> doi <*> url <*> html <*> Just abstract <*> Just mainHtml <*> cit <*> refs <*> figs
                <*> Just toc <*> Just tags <*> Just note <*> Just misc <*> Just userEmail
        where
          m = M.fromList ts
          ft :: Maybe MP.Object -> Maybe T.Text
          ft ma = ma >>= eitherMaybe . MP.tryFromObject
          doi = ft $ M.lookup "doi" m
          url = ft $ M.lookup "url" m
          html = ft $ M.lookup "html" m
          abstract = ft $ M.lookup "abstract" m
          mainHtml = ft $ M.lookup "mainHtml" m
          cit = Just $ fromMaybe emptyCitation $ M.lookup "cit" m >>= citationFromMsgPack
          refs = Just [[]] -- Stub -- f $ M.lookup "url" m
          figs = Just [] -- Stub -- f $ M.lookup "figs" m
          toc = ft $ M.lookup "toc" m
          tags = g $ M.lookup "tags" m -- f $ M.lookup "tags" m
            where
              g ma = fromMaybe [] $ ma >>= eitherMaybe . MP.tryFromObject
          note = ft $ M.lookup "note" m
          misc = h $ M.lookup "misc" m
            where
              h ma = fromMaybe "" $ ma >>= eitherMaybe . MP.tryFromObject
          userEmail = Nothing
    Left err -> Nothing 

msgPackFromCitation :: Citation -> MP.Object
msgPackFromCitation c
  = toObject -- $ M.fromList
      [("doi"::String,toObject $ citationDoi c),("title",toObject $ citationTitle c),
      ("journal",toObject (citationJournal c)),("year", toObject $ citationYear c),
      ("volume",toObject (citationVolume c)),("pageFrom",toObject (citationPageFrom c)),
      ("pageTo", toObject (citationPageTo c)), ("authors", toObject (citationAuthors c)),
      ("publisher", toObject (citationPublisher c)),("type",toObject (citationType c))
       ]

citationFromMsgPack :: MP.Object -> Maybe Citation
citationFromMsgPack obj =
  case MP.tryFromObject obj :: Either String [(String,MP.Object)] of
    Right ts -> Just (emptyCitation{citationDoi = fromJust doi})
    
 --  Right ts -> Citation <$> doi <*> url <*> title <*> journal <*> year <*> volume <*> pageFrom <*> pageTo
   --                  <*> authors <*> publisher <*> ptype
      where
        m = M.fromList ts
        ft :: Maybe MP.Object -> Maybe (Maybe T.Text)
        ft ma = Just (ma >>= eitherMaybe . MP.tryFromObject)
        doi = ft $ M.lookup "doi" m
        url = ft $ M.lookup "url" m
        title = ft $ M.lookup "title" m
        journal = ft $ M.lookup "journal" m
        year = g $ M.lookup "year" m
          where
            g :: Maybe MP.Object -> Maybe (Maybe Int)
            g ma = fromMaybe (Just Nothing) (ma >>= eitherMaybe . MP.tryFromObject)
        volume = ft $ M.lookup "volume" m
        pageFrom = ft $ M.lookup "pageFrom" m
        pageTo = ft $ M.lookup "pageTo" m
        authors = h $ M.lookup "authors" m
          where
            h :: Maybe MP.Object -> Maybe [Text]
            h ma = Just $ fromMaybe [] (ma >>= eitherMaybe . MP.tryFromObject)
        publisher = ft $ M.lookup "publisher" m
        ptype = ft $ M.lookup "type" m
    Left _ -> Nothing

msgPackFromFigure :: Figure -> MP.Object
msgPackFromFigure a = toObject [("figId"::String,figureFigId a),("name",figureName a),("annot",figureAnnot a),("img",figureImg a)]

msgPackFromReference :: Reference -> MP.Object
msgPackFromReference a = toObject [("refId"::String,toObject $ referenceRefId a),("refName",toObject $ referenceRefName a),("cit",maybe ObjectNil msgPackFromCitation (referenceCit a)),("url",toObject $ referenceUrl a)]

-}

