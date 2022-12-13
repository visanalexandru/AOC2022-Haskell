import Data.Char
import Data.List

priority :: Char -> Int
priority x 
 | (isUpper x) = ord x - ord 'A' + 27
 | otherwise =  ord x - ord 'a' + 1

first_half :: String -> String
first_half x = take (div (length x) 2) x

second_half:: String -> String
second_half x = drop (div (length x) 2) x


score1 :: String -> Int
score1 x = sum $ nub [priority x | x<-a , elem x b]
 where 
  a = first_half x
  b = second_half x
 

score2 :: String -> String -> String-> Int
score2 a b c = head $ nub [priority x | x<-a , elem x b, elem x c] 

solve1 :: [String] ->Int
solve1 lines = sum $ map score1 lines

solve2 :: [String] -> Int
solve2 [] = 0
solve2 (a:b:c:d) = score2 a b c + solve2 d 


main = do
 input <- readFile "input"
 let l = lines input
 putStrLn $ show $ solve1 l
 putStrLn $ show $ solve2 l
