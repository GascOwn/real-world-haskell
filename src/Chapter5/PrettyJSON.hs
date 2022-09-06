module Chapter5.PrettyJSON where

import Chapter5.SimpleJSON
import Data.Bits (shiftR, (.&.))
import Numeric (showHex)
import Data.Char (ord)

data Doc = Empty
           | Char Char
           | Text String
           | Line
           | Concat Doc Doc
           | Union Doc Doc
           deriving (Show,Eq)

--string :: String -> Doc
--string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text "" = Empty
text str = Text str

char :: Char -> Doc
char = Char 


double :: Double -> Doc
double d = text (show d)

{-renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str-}

(<>) :: Doc -> Doc -> Doc
Empty <> b = b
a <> Empty = a
a <> b = Concat a b

hcat :: [Doc] -> Doc
hcat xs = undefined

--enclose :: Char -> Char -> Doc -> Doc
--enclose left right x = left Chapter5.PrettyJSON.<> x Chapter5.PrettyJSON.<> right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
  Just r -> text r
  Nothing | mustEscape c -> hexEscape c
          | otherwise -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

smallHex :: Int -> Doc
smallHex x = text "\\u" Chapter5.PrettyJSON.<> text (replicate (4 - length h) '0') Chapter5.PrettyJSON.<> text h
  where h = showHex x ""
  
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) Chapter5.PrettyJSON.<> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff
        
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
  where d = ord c

empty :: Doc
empty = Empty

--series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
--series open close item = enclose open close . fsep . punctuate (char ',') . map item