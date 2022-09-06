module Main where

import System.Environment (getArgs)
--import Chapter2.Exercises
import qualified Chapter3.Types as C3
--import Chapter3.Misc
import Chapter3.Exercises
import Chapter4.Exercises

main :: IO ()
main = print "ble"
  
  
  
  {-mainWith myFunction
         where mainWith function = do
                args <- getArgs
                case args of
                    [input,output] -> interactWith function input output
                    _ -> putStrLn "error: exactly two arguments needed"-}
                    
       {-print $ lastButOne "randomstring"
       print consList
       print $ C3.toList consList
       print $ maybeTree 3 4
       print $ maybeTree "first argument" "second argument"
       print $ pluralise "cat" [0, 1, 745]
       print $ myLength [1,2,3,4]
       print $ means [37, 297, 4, 5]
       print palindrome
       print $ isPalindrome palindrome
       print $ sortListOfLists [[1,2], [1], [1,2,3], [2], [3,4,5,6]]
       print $ treeHeight sillyTree -}
       --print $ splitWith odd [2,3,4,5,6,7,8,9,10]
       -- print $ asIntEither "3.5"
       -- print $ asIntEither "92233720368547758099"

                    

palindrome :: [Integer]
palindrome = makePalindrome [1,2,3]

consList :: C3.List Integer
consList = C3.fromList [1,2,3,4]

maybeTree :: a -> a -> C3.MaybeTree a
maybeTree a b = C3.MaybeTree (Just a) (Just $ C3.MaybeTree (Just b) Nothing Nothing) Nothing

sillyTree :: C3.Tree Integer
sillyTree = C3.Node 1 (C3.Node 2 C3.Empty (C3.Node 4 (C3.Node 5 C3.Empty C3.Empty) C3.Empty)) (C3.Node 3 (C3.Node 4 C3.Empty C3.Empty) C3.Empty)