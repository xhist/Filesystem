import Operations
import Utility

main :: IO ()
main = do
    parseLine ["/"] currentWorkingTree

getLines :: IO String
getLines = do 
    x <- getLine
    if x == "."
        then return ""
        else do 
            xs <- getLines
            return (x ++ "\n" ++ xs)

test1 :: IO()
test1 = do
    content <- getLines
    print content

parseLine::[String]->TreeFS String->IO ()
parseLine wd tree = do
    putStr "$> "
    cmd <- getLine
    case words cmd of
        ["pwd"] -> putStrLn (getPwd wd)
        ("args" : args) -> putStrLn (show args)
        ("cd" : args) -> 
            case args of
                [x] -> parseLine (changeDirectory wd (head args) tree) tree
                _   -> pure ()
        ("ls" : args) ->
            case args of
                [x] -> if nonEmptyDirectory == "" then pure() else putStrLn (nonEmptyDirectory)
                [] -> if emptyDirectory == "" then pure() else putStrLn (emptyDirectory)
                _ -> pure()
                where emptyDirectory = ls wd "" tree
                      nonEmptyDirectory = ls wd (head args) tree
        ("cat" : args) ->
            case args of 
                []  -> pure ()
                _   -> 
                    if not (">" `elem` args)
                        then putStrLn content
                        else 
                            if countOccurences ">" args > 1
                                then pure ()
                                else
                                    if (getElement ">" args == 0)
                                    then do
                                        cont <- getLines
                                        parseLine wd (addContentToFiles (drop ((getElement ">" args) + 1) args) (customInit cont) wd tree)
                                    else parseLine wd (addContentToFiles (drop ((getElement ">" args) + 1) args) contentToAdd wd tree)
                        
                    where content = concatFileContent args wd tree
                          contentToAdd = concatFileContent (take ((getElement ">" args) + 1) args) wd tree 
        ("rm" : args) ->
            case args of 
                []  -> pure()
                _   -> parseLine wd (removeFiles args wd tree)
    parseLine wd tree