module Chapter7.Actions where
    
import Data.Char (toUpper)

-- The IO Monad defines action, they do nothing when they are defined, but perform the task when invoked.
-- Monads are a way of chaining fuctions together purely

str2Action :: String -> IO ()
str2Action input = putStrLn ("Data: " ++ input)

list2Actions :: [String] -> [IO ()]
list2Actions = map str2Action

actions :: [IO ()]
actions = list2Actions $ map show [1..10]

printAll :: IO ()
printAll = runall actions

runall :: [IO ()] -> IO ()
runall [] = return ()
runall (x:xs) = do
    x
    runall xs

doitall :: IO ()
doitall = do
    str2Action "Start"
    printAll
    str2Action "Done!"

doItBetter :: IO ()
doItBetter = do 
    str2Action "Start"
    mapM_ (str2Action . show) [1..10]
    str2Action "Done!"

-- >> sequences two actions together: the first is performed, then the second, the result being the result of the second.
-- This is similar to having a line in a do block
twoLines :: IO ()
twoLines = putStrLn "Line 1" >> putStrLn "Line 2"

-- >>= runs an action, passes the result to a function that returns an action, which is then run (a pipeline in Elixir)
getOneLine :: IO ()
getOneLine = getLine >>= putStrLn 

-- BASIC IO EXAMPLES WITH DO AND NO DO:

basicIO :: IO ()
basicIO = do
    putStrLn "Greetings! What is your name?"
    inpStr <- getLine
    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

basicIONoDo :: IO ()
basicIONoDo = putStrLn "Greetings! What is your name?" >> getLine >>= (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")

-- return in Haskell is different from impeartive languages, it wraps pure data in a monad.  
isGreen :: IO Bool
isGreen = do 
    putStrLn "Is green your favorite color?"
    inpStr <- getLine
    return ((toUpper . head $ inpStr) == 'Y')
