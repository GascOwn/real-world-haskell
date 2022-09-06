module Chapter3.Misc where

import qualified Chapter3.Types as C3

-- ERRORS
-- The standard function error prints a string when an error occurs
mySecond :: [a] -> a
mySecond xs = if null (tail xs) then error "list too short" else head (tail xs)

-- This doesn't allow distinction between recoverable and unrecoverable errors, so it's better to use Maybe
safeSecond :: [a] -> Maybe a
safeSecond (_:x:_) = Just x
safeSecond _ = Nothing

-- LOCAL VARIABLES
-- Local variables can be introduced in a function with "let", the main function follows after "in"

lend :: (Ord a, Num a) => a -> a -> Maybe a
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                          in if balance < reserve then Nothing else Just newBalance

-- Another way of introducing local variables is the "where" clause
lendWithWhere :: (Ord a, Fractional a) => a -> a -> Maybe a
lendWithWhere amount balance = if balance < (reserve * 0.5) then Nothing else Just newBalance
                               where reserve = 100
                                     newBalance = balance - amount

-- Functions can be defined as well
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
  where plural n = case n of
                   0 -> "no " ++ word ++ "s"
                   1 -> "one " ++ word
                   _ -> show n ++ " " ++ word ++ "s"

-- Guards provide pattern matching with conditional evaluation
nodesAreSame :: Eq a => C3.Tree a -> C3.Tree a -> Maybe a
nodesAreSame (C3.Node a _ _) (C3.Node b _ _)
  | a == b = Just a
  | otherwise = Nothing

lendWithGuards :: (Ord a, Fractional a) => a -> a -> Maybe a
lendWithGuards amount balance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance
  where reserve = 100
        newBalance = balance - amount

myDropWithGuards :: (Ord t, Num t) => t -> [a] -> [a]
myDropWithGuards n xs | n <= 0 = xs
myDropWithGuards _ [] = []
myDropWithGuards n (_:xs) = myDropWithGuards (n - 1) xs

