module PropertiesTest where 

{- ici j'effectue les tests de proprietés
    - la fonction de test peut avoir n'importe quelle nom, a la difference du test unitaire où il faut qu'elle s'appelle main
    - la fonction de test peut prendre des trucs en entree, a la difference de l'autre ou la fonction a seulement pour type : IO () qui est le type de la fonction hspec
    - pour lancer le test on fait : on appelle òla fonction quickCheck en lui passant la fonction de test
-}
import Test.QuickCheck
import Data.List 

newtype TestProp a = TestProp a deriving (Eq, Ord, Show)

testPropGen :: (Arbitrary a) => Gen (TestProp a)
testPropGen = do
    x <- arbitrary
    return (TestProp x)

instance (Arbitrary a ) => Arbitrary (TestProp a) where 
    arbitrary = testPropGen


functionTesting :: Gen (TestProp String)
functionTesting = testPropGen


--function qui teste un des propietes des listes avec un type generique
testprop1 :: Eq a => [a] -> Bool
testprop1 xs = Data.List.reverse (reverse xs) == xs 
    -- where types = xs :: [Int]

-- function qui teste la propirte des liste avec mon type personnalisé TestProp a
testprop2 :: [TestProp Int] -> Bool
testprop2 ys = reverse (reverse ys) == ys



fails :: [Int] -> Bool
fails xs = Data.List.reverse xs == xs 


main :: IO ()
main = do 
    quickCheck propAddition
    quickCheck propAddition2
    quickCheck propAddition3
-------------------------------------------------------------
-- | Helpers functions for testing 
-------------------------------------------------------------
--  ici on teste la commutativité de la fonction addition
propAddition a b = addition a b == addition b a

--  ici on teste l'associativité de la fonction addition 
propAddition2 a b c = addition (addition a c) b == addition  a (addition b c)
--  ici on teste la neutralité de la fonction addition
propAddition3 a = addition a 0 == a 


