module IoWithYannick where

import System.IO

main :: IO ()
main = do
    putStrLn ("Which file do you want to read ?")
    fich81 <- getLine
    putStrLn ("In which one do you want to write ?")
    fich102 <- getLine
    inh <- openFile fich81 ReadMode
    outh <- openFile fich102 WriteMode
    mainloop inh outh
    hClose inh
    hClose outh

myfun0 :: String -> Int -> [String]
myfun0 [] _ = []
myfun0 xs nbr = let u = take nbr xs
                    m = last u
                    v = drop (length u) xs
                 in if (m == ' ')
                        then u : myfun0 v nbr
                    else let u1 = reverse u
                             --v1 = dropWhile (\t -> t == ' ') u1
                             v1 = takeWhile (/= ' ') u1
                             --s0 = reverse v1
                             z = nbr - (length v1)
                             --s1 = drop (length s0) xs
                             (a,b) = splitAt z xs
                         in a : myfun0 b nbr


                      

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
    ineof  <- hIsEOF inh
    if ineof
        then return ()
    else do
        inpStr <- hGetContents inh
        --let --inpStr0 = filter (/= '\n') inpStr
            --someVar = lines inpStr
        putStrLn ("In how much character do you want to break ?")
        numbr <- getLine
        let 
            t = read numbr :: Int
            s = myfun0 inpStr t
        hPutStrLn outh (unlines s)