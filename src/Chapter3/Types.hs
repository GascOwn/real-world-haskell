module Chapter3.Types where

import Data.Maybe


data BookInfo = Book Int String [String] deriving Show
data MagazineInfo = Magazine Int String [String] deriving Show

book1 :: BookInfo
book1 = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

-- ALGEBRAIC DATA TYPES
-- Sum data types are types that have alternatives, can only be one of many
-- Product data types are the combination of multiple data types

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                   deriving Show

-- Enumerators are very easy to write

data Colours = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
             deriving (Eq, Show)

-- Unions are similarly easy
type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]

-- Record syntax helps access fields
data Customer = Customer { customerID :: CustomerID
                         , customerName :: String
                         , customerAddress :: Address }
                         deriving Show

-- When using record syntax to create a type, both ways can be used to instantiate it
customer1 :: Customer
customer1 = Customer 12345 "Daniele Anselmo" ["Via Silvio Pellico 2", "16035", "Rapallo", "GE"]

-- Record syntax allows to write fields in a different order
customer2 :: Customer
customer2 = Customer { customerID = 67890
                     , customerAddress = ["Via San Bartolomeo del Fossato 102"
                                           , "16149"
                                           , "Genova"
                                           , "GE"]
                     , customerName = "Miriam Paparatto" }

-- record syntax gives accessor functions with names equal to those of the fields they access
listOfClientNames :: [String]
listOfClientNames = [customerName customer1, customerName customer2]

-- PARAMETERISED TYPES
-- These types have a type variable in their declaration, which is another type

data Perhaps a = Simply a | ShitAll


parameterizedString :: Perhaps String
parameterizedString = Simply "Your mom's a whore"

parameterizedInt :: Perhaps Int
parameterizedInt = Simply 666

-- RECURSIVE TYPES
-- These types are defined in terms of themselves

data List a = Cons a (List a) | Nil deriving Show

recursiveList :: List Integer
recursiveList = Cons 1 (Cons 2 (Cons 3 Nil))

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: List a -> [a]
toList (Cons a b) = a : toList b
toList Nil = []

-- Trees are recursive structures

data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show
data MaybeTree a  = MaybeTree (Maybe a) (Maybe (MaybeTree a)) (Maybe (MaybeTree a)) deriving Show

dataTree :: Num a => Tree a
dataTree = Node 3 Empty Empty


