module Chapter4.OnLists where


-- functions and datatypes can also be written with infix notation using backticks, this can help readability

plus :: Num a => a -> a -> a
a `plus` b = a + b

data a `Pair` b = a `Pair` b deriving (Show)

-- Lists are essential to functional programming, there are plenty of functions that work with them, here are some:

randomList :: [Int]
randomList = [1,2,3]

sHead = head randomList -- first element
sTail = tail randomList -- everything but first
sInit = init randomList -- everything but last
sLast = last randomList -- last element
sLength = length randomList -- length of list
sNull = null randomList -- checks if list is empty
sAll = all odd randomList -- checks if all elements return true with given function
sAny = any odd randomList -- checks if any element of the list return true with given function
sConcat = randomList ++ [4,5,6] -- turns list of lists into a list

-- Haskell has no loops, it has to rely on explicit recursion or mapping functions



-- functions that return only a certain part of the string

splitLines :: [Char] -> [[Char]]
splitLines [] = []
splitLines cs = let (pre, suf) = break isLineTerminator cs
                in pre : case suf of
                   ('\r':'\n':rest) -> splitLines rest
                   ('\r':rest) -> splitLines rest
                   ('\n':rest) -> splitLines rest
                   _ -> []
                where isLineTerminator c = c == '\r' || c == '\n'


