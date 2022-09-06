{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module Chapter5.SimpleJSON(
    JValue (..) -- this exports the datatype and all the data constructor 
  , getString
  , getInt
  , getBool
  , getDouble
  , getArray
  , getObject
  , isNull
) where

import Data.List (intercalate)

data JValue = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

-- accessor functions

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _ = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

-- rendering function

renderJValue :: JValue -> String
renderJValue value = case value of
  JString s -> show s
  JNumber n -> show n
  JBool True -> "true"
  JBool False -> "false"
  JNull -> ""
  JObject o ->  "{" ++ pairs o ++ "}"
    where pairs [] = []
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k, v) = show k ++ ": " ++ renderJValue v
  JArray a -> "[" ++ values a ++ "]"
    where values [] = []
          values vs = intercalate ", " (map renderJValue vs)

-- it is good practice to separate pure code from IO code

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)