module Chapter8.EfficientFileProcessing where

import qualified Data.ByteString.Lazy as L
import Text.Regex.Posix ( (=~) )

-- Being String the default type for reading files, its lack of efficiency will make the program perform badly
processBadly :: IO ()
processBadly = do
    contents <- getContents
    print (sumFile contents)
    where sumFile = sum . map read . words

-- A ByteString is more efficient 

-- file: ch08/ElfMagic.hs

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

regexFoo :: Bool
regexFoo = "what the fuck" =~ "Foo" :: Bool