module Chapter3.Exercises where

import Data.List
import Data.Ord (comparing)
import Chapter3.Types

myLength :: [a]  -> Int
myLength list = case list of
  [] -> 0
  (_:xs) -> 1 + myLength xs

means :: Integral a => [a] -> Double
means list
  | null list = 0
  | otherwise = fromIntegral (sumOfList list) / fromIntegral (myLength list)
      where sumOfList l = case l of
               [] -> 0
               (x:xs) -> x + sumOfList xs

makePalindrome :: [a] -> [a]
makePalindrome list = case list of
  [] -> []
  l -> l ++ reverse l

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

sortListOfLists :: [[a]] -> [[a]]
sortListOfLists = sortOn length

treeHeight :: Tree a -> Int
treeHeight tree = case tree of
  Empty -> 0
  Node _ left right -> max a b
    where a = 1 + treeHeight left
          b = 1 + treeHeight right


data Direction = DirLeft | Straight | DirRight deriving Show
data Point = Point Int Int

calculateDirection :: Point -> Point -> Point -> Direction
calculateDirection (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | crossProduct point1 point2 > 0 = DirRight
    | crossProduct point1 point2 < 0 = DirLeft
    | otherwise = Straight
    where crossProduct (Point xp1 yp1) (Point xp2 yp2) = (xp1 - yp2) * (xp2 - yp1)
          point1 = Point (x3 - x1) (y3 - y1)
          point2 = Point (x2 - x1) (y2 - y1)

calculateListDirection :: [Point] -> [Direction]
calculateListDirection [] = []
calculateListDirection (p:ps) = calculateDirection p (head ps) (head (tail ps)) : calculateListDirection ps
