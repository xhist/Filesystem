module Operations where

import Data.Maybe (catMaybes)
data TreeFS a = EmptyTree
                | File a a
                | Directory a [(TreeFS a)]
                deriving (Eq, Ord, Show, Read)

currentWorkingTree::TreeFS String
currentWorkingTree = Directory "/" [Directory "first" [Directory "scheme" [], Directory "haskell" [File "file1" "example"]], Directory "second" [], File "file1" ""]

getPwd::[String] -> String
getPwd path
    | length path == 0  = "/"
    | otherwise         = if length path == 1 then head path else concat (tail (map ("/"++) path))


findDirectory::String->[(TreeFS String)]->TreeFS String
findDirectory _ []                          = EmptyTree
findDirectory d ((File name cont):y)        = findDirectory d y
findDirectory d ((Directory name s):y)
    | d == name     = Directory name s
    | otherwise     = findDirectory d y

findFile::String->[(TreeFS String)]->TreeFS String
findFile _ []                               = EmptyTree
findFile filename ((Directory name s):y)    = findFile filename y 
findFile filename ((File name cont):y)
            | filename == name  = File name cont
            | otherwise         = findFile filename y

getDirectoryChildren::TreeFS String->[(TreeFS String)]
getDirectoryChildren (Directory s children) = children
getDirectoryChildren _                      = []

getPath::[String]->TreeFS String->TreeFS String
getPath [] _                = EmptyTree
getPath (p:[]) (Directory s c)
        | p == s            = Directory s c
        | otherwise         = EmptyTree
getPath _  (EmptyTree)      = EmptyTree
getPath _  (File name cont) = EmptyTree
getPath (h:t) (Directory s c)
        | h == s    = if length result == 0 then EmptyTree else head result
        | otherwise = EmptyTree
        where result = (filter (\y -> y /= EmptyTree) (map (\x -> getPath t x) c))

-- getPath::[String]->TreeFS String->TreeFS String
-- getPath [] x               = x
-- getPath _  (EmptyTree)      = EmptyTree
-- getPath _  (File name cont) = EmptyTree
-- getPath (h:t) (Directory s c)
--         | result /= EmptyTree = getPath t result
--         | otherwise           = EmptyTree
--         where result = findDirectory h c

newDirectory::[String]->String->[String]
newDirectory wd newD
    | length newD == 0                              = wd
    | length newD == 2 && (take 2 newD) == ".."     = if (length wd > 1) then (init wd) else wd
    | (take 3 newD) == "../"                        = newDirectory (if length wd > 1 then (init wd) else wd) (drop 3 newD)
    | (take 2 newD) == "./"                         = newDirectory wd (drop 2 newD)
    | newD !! 0 == '/'                              = newDirectory ["/"] (tail newD)
    | (newD !! 0 /= '/' && newD !! 0 /= '.')        = newDirectory (wd ++ [result]) (drop (length result + 1) newD)
    | otherwise                                     = wd
    where result = takeWhile (/= '/') newD

changeDirectory::[String]->String->TreeFS String->[String]
changeDirectory wd newD tree
        | getPath result tree /= EmptyTree                   = result
        | otherwise                                          = wd
        where result = newDirectory wd newD

getContent::[(TreeFS String)]->[String]
getContent []                   = []
getContent ((File s cont):y)    = s:getContent y
getContent ((Directory s l):y)  = s:getContent y

getPathContent::[String]->String->TreeFS String->[String]
getPathContent path newPath tree
        | newPathTree /= EmptyTree = getContent (getDirectoryChildren newPathTree)
        | otherwise                = getContent (getDirectoryChildren previousPathTree)
        where newPathTree       = getPath (newDirectory path newPath) tree
              previousPathTree  = getPath path tree

ls::[String]->String->TreeFS String->String
ls path newPath tree = if length result > 0 then init result else ""
        where result = (concat (map (\x -> x ++ " ") (getPathContent path newPath tree)))

getFileName::String->String
getFileName filePath = reverse (takeWhile (/= '/') (reverse filePath))

getFilePath::String->[String]->[String]
getFilePath filePath wd= newDirectory wd (reverse (drop (length result) (reverse filePath)))
                where result = getFileName filePath

getFile::String->[String]->TreeFS String->TreeFS String
getFile filePath wd tree
        | result /= EmptyTree = (findFile (getFileName filePath) (getDirectoryChildren result))
        | otherwise           = EmptyTree
        where result = getPath (getFilePath filePath wd) tree

getFiles::[String]->[String]->TreeFS String->[(TreeFS String)]
getFiles filePaths wd tree = map (\x -> getFile x wd tree) filePaths

extractFilePaths::String->[String]
extractFilePaths filePaths
        | length filePaths == 0     = []
        | otherwise                 = result:extractFilePaths (drop (length result + length droppedResult) filePaths)
            where droppedResult = takeWhile (== ' ') filePaths
                  result = takeWhile (/= ' ') (dropWhile (== ' ') filePaths)

firstJust::[Maybe a]->Maybe a
firstJust [] = Nothing
firstJust (Nothing : ps) = firstJust ps
firstJust (Just a : ps)  = Just a

getFileContent::[String]->TreeFS String->String->Maybe String
getFileContent [] EmptyTree name = Nothing
getFileContent [] (File s c) name
        | name == s = Just c
        | otherwise = Nothing
getFileContent [] (Directory s l) name = Nothing
getFileContent (p:ps) EmptyTree _ = Nothing
getFileContent (p:ps) (File s c) _ = Nothing
getFileContent (p:ps) (Directory s l) name
        | p == s    = firstJust (map (\x -> getFileContent ps x name) l)
        | otherwise = Nothing

sameFile::String->TreeFS String->Bool
sameFile name (File s c) = name == s
sameFile name _          = False

addFile::[String]->String->String->TreeFS String->TreeFS String
addFile [] name content EmptyTree = File name content
addFile [] name content (File s c) = File s c
addFile [] name content (Directory s l) = Directory s l
addFile (p:[]) name content (Directory s l)
        | p == s    =  Directory s ((filter (not . sameFile name) l) ++ [File name content])
        | otherwise =  Directory s l
addFile (p:ps) name content EmptyTree = EmptyTree
addFile (p:ps) name content (File s c) = File s c
addFile (p:ps) name content (Directory s l)
        | p == s    = Directory s (map (\x -> addFile ps name content x) l)
        | otherwise = Directory s l

--catOneFile::[String]->String->TreeFS String->TreeFS String
--catOneFile path name tree = case getFileContent path tree name of  
--                                Nothing -> 

getFilesContent::[String]->[String]->TreeFS String->[String]
getFilesContent l wd tree = catMaybes (map (\x -> getFileContent (getFilePath x wd) tree (getFileName x)) l)

concatFileContent::[String]->[String]->TreeFS String->String
concatFileContent filePaths wd tree = if length result > 0 then init result else result
            where result = concat (map (\x-> x ++ "\n") (getFilesContent filePaths wd tree))

addContentToFiles::[String]->String->[String]->TreeFS String->TreeFS String
addContentToFiles [] content wd tree        = tree
addContentToFiles (x:xs) content wd tree    = addContentToFiles xs content wd result
            where result = addFile (getFilePath x wd) (getFileName x) content tree

removeFile::[String]->String->TreeFS String->TreeFS String
removeFile [] name EmptyTree = EmptyTree
removeFile [] name (File s c) = File s c
removeFile [] name (Directory s l) = Directory s l
removeFile (p:[]) name (Directory s l)
        | p == s    =  Directory s (filter (not . sameFile name) l)
        | otherwise =  Directory s l
removeFile (p:ps) name EmptyTree = EmptyTree
removeFile (p:ps) name (File s c) = File s c
removeFile (p:ps) name (Directory s l)
        | p == s    = Directory s (map (\x -> removeFile ps name x) l)
        | otherwise = Directory s l

removeFiles::[String]->[String]->TreeFS String->TreeFS String
removeFiles [] wd tree          = tree
removeFiles (x:xs) wd tree      = removeFiles xs wd result
            where result = removeFile (getFilePath x wd) (getFileName x) tree