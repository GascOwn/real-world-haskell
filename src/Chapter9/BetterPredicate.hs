{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use withFile" #-}

module Chapter9.BetterPredicate where 

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, getFileSize)
import Data.Time.Clock.System
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Chapter9.RecursiveContents (getRecursiveContents)
import GHC.IO.Buffer (checkBuffer)
import GHC.Generics (Constructor(conName))

-- The type predicate is a synonym for a function of four arguments
type Predicate = FilePath-> Permissions -> Maybe Integer -> SystemTime -> Bool 

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            permissions <- getPermissions name 
            size <- System.Directory.getFileSize name 
            modified <- getModificationTime name 
            return (p name permissions size (utcToSystemTime modified))


simpleFileSize :: FilePath -> IO (Maybe Integer)
simpleFileSize path = handle (\_ -> return Nothing) $ do 
    h <- openFile path ReadMode
    size <- hFileSize h 
    hClose h
    return (Just size)

-- bracket makes it so that hClose will always be called if openFile succeeds.
-- bracket takes three argument: 1 - an action that acquires a resource 2 - an action that releases the resource 3 - an action that runs in betweem 
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\_ -> return Nothing) $
    bracket (openFile path ReadMode) hClose  (\h -> do
        size <- hFileSize h
        return (Just size))

