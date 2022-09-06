module Chapter4.FunctionApplication where

import GHC.OldList (isInfixOf, tails)
import GHC.Unicode (isSpace, isUpper)


-- anonymous functions are functions with no name, they can be used in place of helper functions

isInAny :: (Foldable t, Eq a) => [a] -> t [a] -> Bool
isInAny needle haystack = any inSequence haystack
  where inSequence s = needle `isInfixOf` s


isInAny2 needle haystack = any (\s -> isInfixOf needle s) haystack


-- Partial application is giving a function less arguments than it should have, or add a fixed argument to a function

dropWhileIsSpace :: String -> String
dropWhileIsSpace = dropWhile isSpace

mappeddropWhileIsSpace :: [String]
mappeddropWhileIsSpace = map dropWhileIsSpace [" a","f"," e"]

isInAny3 :: (Foldable t, Eq a) => [a] -> t [a] -> Bool
isInAny3 needle haystack = any (isInfixOf needle) haystack

-- Sections allow us to write partial application in infix style (here the 3 is the second argument of (*) x 3)

multiplyListX3 :: [Integer] -> [Integer]
multiplyListX3 = map (*3)

multipliedList :: [Integer]
multipliedList = multiplyListX3 [34, 32, 23, 95]

-- As Patterns allow to bind a variable to what follows the @ symbol

suffixes :: [a] -> [[a]]
suffixes listT@(_:xs) = listT : suffixes xs
suffixes _ = []

-- Composition is running the output of a function as input of the following. Composition is right associative

suffixesComp :: [a] -> [[a]]
suffixesComp = init . tails

-- This let's us create new functions. The following counts how many words in a string start with a capitol letter

capCount :: String -> Int
capCount = length . filter (isUpper . head) . words