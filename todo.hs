import System.Environment
import System.Directory
import System.Console.ANSI
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("interactive", interactive)
            ]            

add :: [String] -> IO ()
add (fileName : todoItem) = appendFile fileName $ (unwords todoItem) ++ "\n"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith showTodoLine [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberStr] = do
    contents <- readFile fileName
    let number = read numberStr
        todoTasks = lines contents
        newTodoTasks = delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle $ unlines newTodoTasks
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
    
showTodoLine :: Int -> String -> String
showTodoLine n todoStr =
    setSGRCode [SetColor Foreground Vivid Red] ++
    show n ++ 
    setSGRCode [SetConsoleIntensity FaintIntensity] ++
    " - " ++
    setSGRCode [Reset] ++
    todoStr 

interactive :: [String] -> IO ()
interactive [fileName] = do
    clearScreen
    view [fileName]

main = do
    (command:args) <- getArgs
    case lookup command dispatch of
        (Just action) -> action args
        Nothing -> putStrLn $ "Error: command '" ++ command ++ "' not found" 

