import System.Environment
import System.Directory
import System.Console.ANSI
import System.IO
import Data.List
-- import UI.NCurses

dispatch :: [(String, FilePath -> [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("interactive", interactive)
            ]            

add :: FilePath -> [String] -> IO ()
add fileName todoItem = do 
    item <- case todoItem of 
        [] -> prompt "Item to add: " 
        xs -> return $ unwords xs
    appendFile fileName $ item ++ "\n"
    
view :: FilePath -> [String] -> IO ()
view fileName _ = do
    contents <- readFile fileName
    sequence_ $ zipWith showTodoLine [0..] $ lines contents

remove :: FilePath -> [String] -> IO ()
remove fileName numberStr = do
    item <- case numberStr of 
        [] -> prompt "# to remove: "
        x:_ -> return x
    contents <- readFile fileName
    let number = read item
        todoTasks = lines contents
        newTodoTasks = delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle $ unlines newTodoTasks
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
    
showTodoLine :: Int -> String -> IO ()
showTodoLine n todoStr = do
    setSGR [SetColor Foreground Vivid Red]
    putStr $ show n
    setSGR [SetConsoleIntensity FaintIntensity]
    putStr " - "
    setSGR [Reset]
    putStrLn todoStr 

interactive :: FilePath -> [String] -> IO ()
interactive fileName _ = do
    clearScreen
    title $ "TODO " ++ fileName
    view fileName []
    putStrLn ""
    command <- getCommandChar "Enter command (a)dd / (r)emove / (q)uit"
    case command of
        'a' -> add fileName [] >> interactive fileName []
        'r' -> remove fileName [] >> interactive fileName []
        'q' -> return ()
        _ -> interactive fileName []

prompt :: String -> IO String
prompt str = do
    setSGR [SetColor Foreground Vivid Red]
    putStr str 
    hFlush stdout
    setSGR [Reset]
    item <- getLine 
    return item

getCommandChar :: String -> IO Char
getCommandChar str = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn str
    setSGR [Reset]
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    command <- getChar
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    return command

title :: String -> IO ()
title str = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn str
    setSGR [Reset]
    putStrLn ""

main :: IO ()
main = do
    (fileName : command : args) <- getArgs
    case lookup command dispatch of
        (Just action) -> action fileName args
        Nothing -> putStrLn $ "Error: command '" ++ command ++ "' not found" 

