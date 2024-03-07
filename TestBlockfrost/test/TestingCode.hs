module TestingCode where


import Test.QuickCheck
import Functions

prop_addition :: Int -> Int -> Bool 
prop_addition x y = (addition x y) == (x + y)