module Chapter4.Exercises where

import Data.Char (digitToInt, isDigit)
import Data.List


-- Safe list functions

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit list = Just (init list)

-- Split with a predicate

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = (els ++ h) : splitWith f t
  where (els, rest) = span f xs
        (h, t) = if null rest then ([], []) else ([head rest], tail rest)


-- write first word of each line of input
interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)


myFunction :: String -> String
myFunction string = unlines (map each (lines string))
  where each [] = [] 
        each  s = head (words s)
        


asInt :: String -> Int
asInt [] = 0
asInt string = loop 0 string where loop acc (x:xs) = let acc' = acc * 10 + digitToInt x in loop acc' xs
                                   loop _ [] = 0

asIntFold :: String -> Int
asIntFold [] = 0
asIntFold (sign:num)
  | sign == '-' = loop 0 num * (-1)
  | otherwise = loop 0 num
  where loop = foldl' (\i c -> i * 10 + digitToInt c)

asIntEither :: String -> Either String Int
asIntEither [] = Right 0
asIntEither (sign:num)
              | null num && not (all isDigit num) = Left "Numbers only"
              | length num >= length (show (maxBound :: Int)) = Left "Number is too large"
              | sign == '-' = Right (loop 0 num * (-1))
              | otherwise = Right (loop 0 num)
              where loop = foldl' (\i c -> i * 10 + digitToInt c)

groupByfold :: (a -> a -> Bool) -> [a] -> [[a]]
groupByfold f = foldr step [[]]
  where step el ([]:t) = [el] : t
        step el acc 
          | f el (head (head acc)) = (el : head acc) : tail acc
          | otherwise = [el] : acc
