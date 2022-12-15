import Data.List

solve :: String -> Int -> Int
solve list to_take  
 | (length $ nub $ take to_take list) == to_take = to_take 
 | otherwise = 1 + solve (tail list) to_take

main = do 
 input <- readFile "input"
 putStrLn $ show $ solve input 4
 putStrLn $ show $ solve input 14
