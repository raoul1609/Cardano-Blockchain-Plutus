
-- le module de testing doit exporter tests
-- le nom de la fonction de testing doit etre tests 

-- module TestingCode  (tests ) where 

module Main  where 

import Test.Hspec 
--import Distribution.TestSuite.QuickCheck 
import Test.QuickCheck 
import Control.Exception

import TestFonctions 



main :: IO ()
main = hspec $ do 
    describe "test de kìla fonction addition " $ do 
        it "(addition a b) >= a" $ do 
            (addition 2 5) >= 2 `shouldBe` True  

-- main :: Int -> Int -> Bool 
-- main x y = (addition x y) == x + y 


-- tests :: IO [Test ]
-- tests = return [testProperty "addition est la fonction dont je veux tester" someProperty ]

-- someProperty :: Integer -> Integer -> Bool 
-- someProperty a b = addition a b == a + b