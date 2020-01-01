import System.Environment
import System.Directory
import System.Console.ANSI
import System.IO
import Data.List
-- import UI.NCurses
-- import qualified Data.Text as T

dispatch :: [(String, FilePath -> [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            -- , ("interactive", interactive)
            ]            

add :: FilePath -> [String] -> IO ()
add fileName todoItem = do 
    item <- case todoItem of 
        [] -> putStrLn "Item to add: " >> getLine
        xs -> return $ unwords xs
    appendFile fileName $ item ++ "\n"
    
view :: FilePath -> [String] -> IO ()
view fileName _ = do
    contents <- readFile fileName
    sequence_ $ zipWith showTodoLine [0..] $ lines contents

remove :: FilePath -> [String] -> IO ()
remove fileName numberStr = do
    item <- case numberStr of 
        [] -> putStrLn "Item to remove: " >> getLine
        x:xs -> return x
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
    putStr todoStr 

-- interactive :: [String] -> IO ()
-- interactive [fileName] = do
--     -- clearScreen
--     -- view [fileName]
--     interactive [fileName]

main :: IO ()
main = do
    (command : fileName : args) <- getArgs
    case lookup command dispatch of
        (Just action) -> action fileName args
        Nothing -> putStrLn $ "Error: command '" ++ command ++ "' not found" 

