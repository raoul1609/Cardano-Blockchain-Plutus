
module TestFonctions where 

-- fonction qui additione deux nombres

addition :: Int -> Int -> Int 
addition = (+) 


--fonction qui fait la multiplication en faisant l'addition recursive

multiplication :: (Eq a, Integral a, Ord a) =>  a -> a -> a 
multiplication x y 
    | y >= 0 = case y of 
        0 -> 0 
        1 -> x 
        _ -> x + multiplication x (y-1)
    | otherwise = case y of 
        -1 -> -x 
        _ -> -1*(multiplication x (abs y))