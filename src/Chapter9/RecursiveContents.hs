module Chapter9.RecursiveContents where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents, pathIsSymbolicLink)
import System.FilePath ((</>))


-- One thing to note here is that an anonymous functions is used as the body of a loop for forM.
-- This is one of the most common uses for anonymous functions
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do 
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
        then getRecursiveContents path
        else return [path]
    return (concat paths)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)