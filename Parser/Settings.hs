module Settings where

rootFolder :: String
rootFolder = "/Users/username/PaperServer/Parser/"
logFile = rootFolder ++ "parser.log"


-- ToDo:
-- In the future, there should be some way to share this func between
-- two projects, Parser and PaperServer.

resourceRootUrl = "/resource/"
