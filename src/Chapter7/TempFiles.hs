{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Chapter7.TempFiles where

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catchIOError)
import Control.Exception(finally)
import Data.Char (toUpper)

myMain :: String -> IO ()
myMain tempFile = withTempFile tempFile myAction

myAction :: [Char] -> Handle -> IO ()
myAction tempname temph = do
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file at " ++ tempname
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++ show (length tempdata) ++ " bytes: " ++ tempdata
    hPutStrLn temph tempdata
    pos <- hTell temph
    putStrLn $ "After writing, my new position is " ++ show pos
    putStrLn "The file content is: " 
    hSeek temph AbsoluteSeek 0
    c <- hGetContents temph
    putStrLn c
    putStrLn "Which could be expressed as this Haskell literal:"
    print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pat func = do
    tempdir <- catchIOError getTemporaryDirectory (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pat
    finally (func tempfile temph) (do
        hClose temph 
        removeFile tempfile)

lazyIO :: IO ()
lazyIO = do
    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    inpStr <- hGetContents  inh
    hPutStr outh $ map toUpper inpStr
    hClose inh 
    hClose outh

-- readFile and writeFile handle all the file opening, reading/writing, and closing
lazyIOBetter :: IO ()
lazyIOBetter = do
    inpStr <- readFile "input.txt"
    writeFile "output.txt" (map toUpper inpStr)

-- interact takes from the standard input and prints to the standard output
interactToUpper :: IO ()
interactToUpper = interact (map toUpper)

-- interact is also used for filtering
onlyWithA :: IO ()
onlyWithA = interact (unlines . filter (elem 'a') . lines)