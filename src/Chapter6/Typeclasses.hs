module Chapter6.Typeclasses where

data Color = Red | Green | Blue

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False
  a /= b = not (a == b)

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Read Color where
readsPrec :: p -> [Char] -> [(Color, [Char])]
readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
  where tryParse [] = []
        tryParse ((attempt, result):xs) = if take (length attempt) value == attempt then [(result, drop (length attempt) value)] else tryParse xs