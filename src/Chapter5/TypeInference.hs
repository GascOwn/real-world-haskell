module Chapter5.TypeInference where

import Data.Char (toUpper)


toUpperFirst :: [Char] -> [Char]
toUpperFirst (x:xs) = toUpper x : xs
toUpperFirst [] = []

toCamelCase :: [Char] -> [Char]
toCamelCase xs = concatMap toUpperFirst (words xs) 