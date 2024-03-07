
-- le module de testing doit exporter tests
-- le nom de la fonction de testing doit etre tests 
-- lorsqu'on fait le test unitaire le nom de la fonction de test est toujours main
-- le test unitaire est limite car il permet de tester uniquement un operation precise d'une fonction

-- module TestingCode  (tests ) where 

module Main  where 

import Test.Hspec 
--import Distribution.TestSuite.QuickCheck 
--import Test.QuickCheck 
--import Control.Exception

import TestFonctions 



-- main :: IO ()
-- main = hspec $ do 
--     describe "test de kìla fonction addition " $ do 
--         it "(addition a b) >= a" $ do 
--             (addition 2 5) >= 2 `shouldBe` True 

-- main :: IO ()
-- main = hspec $ do
--     context "another way to do a test without using describe"  $ do 
--         specify "adiition 2 5 est plus grand que 5" $ do 
--             addition  2 5 >= 5 

-- test unitaire d'une fonction de multiplication 
main :: IO ()
main = do 
    hspec $ do
        describe "test about multiplication regarding recursive addition" $ do 
            it "la multiplication 2 5 est plus grand que 5" $ do 
                (multiplication 2 5) == 5 `shouldBe` True
        
        describe "test de la fonction addition " $ do 
            it "(addition a b) >= a" $ do 
                addition 2 5 >= 2 `shouldBe` True 

        describe "test about multiplication regarding recursive addition" $ do 
            it "la multiplication 4 -1 est plus grand que 5" $ do 
                multiplication 4 (-1) > 0 `shouldBe` False

-- main :: Int -> Int -> Bool 
-- main x y = (addition x y) == x + y 


-- tests :: IO [Test ]
-- tests = return [testProperty "addition est la fonction dont je veux tester" someProperty ]

-- someProperty :: Integer -> Integer -> Bool 
-- someProperty a b = addition a b == a + b 