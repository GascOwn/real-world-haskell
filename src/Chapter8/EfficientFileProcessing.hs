module Chapter8.EfficientFileProcessing where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as CL

-- Being String the default type for reading files, its lack of efficiency will make the program perform badly
processBadly :: IO ()
processBadly = do
    contents <- getContents
    print (sumFile contents)
    where sumFile = sum . map read . words

-- A ByteString is more efficient (ELF is the format for executables on Unix)
-- There is a ByteString and a ByteString.Lazy library.
-- The former stores data in a single array, the latteer in chunks of 64K KB max.
hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

-- The Haskell datatype for representing bytes is Word8 in the Data.Word module
-- L.readFile is the lazy ByteString version of readFile
isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)


-- Data.ByteString.Char8 exposes individual string elements as Char instead of Word8

readCsw :: IO ()
readCsw = putStr =<< readFile "prices.csw"

closing :: CL.ByteString -> Maybe Int
closing = readPrice . (!!4) . CL.split ','

readPrice :: CL.ByteString -> Maybe Int
readPrice str = case CL.readInt str of
    Nothing -> Nothing
    Just (dollars, rest) -> 
        case CL.readInt (L.tail rest) of
            Nothing -> Nothing
            Just (cents, _) -> Just (dollars * 100 + cents)

highestClose :: CL.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . CL.lines

highestCloseFrom :: FilePath -> IO ()
highestCloseFrom path = do
    contents <- CL.readFile path
    print (highestClose contents)





