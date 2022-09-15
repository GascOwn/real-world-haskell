module Chapter9.ExampleDomainSpecificPredicates where
import System.Directory ( Permissions )
import Data.Time.Clock.System ( SystemTime )
import System.FilePath (takeExtension)

-- This is a more generalized version of Predicate
type InfoP a = FilePath -> Permissions -> Maybe Integer -> SystemTime -> a

pathP :: InfoP FilePath
pathP path _ _ _  = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP infoP value filepath ipermissions size lastModified = infoP filepath ipermissions size lastModified == value

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP function infoP value filepath ipermissions size lastModified = function (infoP filepath ipermissions size lastModified) value 

lift2P :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
lift2P function infoP1 infoP2 filepath ipermissions size lastModified = function (infoP1 filepath ipermissions size lastModified) (infoP2 filepath ipermissions size lastModified) 

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = lift2P (&&)

orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
orP = lift2P (||)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool 
simpleAndP infoP1 infoP2 filepath permissions size lastModified = infoP1 filepath permissions size lastModified && infoP2 filepath permissions size lastModified

liftPath :: (FilePath -> a) -> InfoP a 
liftPath f w _ _ _ = f w

(==?) :: InfoP String -> String -> InfoP Bool
(==?) = equalP

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

(>?) :: InfoP Integer -> Integer -> InfoP Bool
(>?) = greaterP

infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest :: InfoP Bool
myTest = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072