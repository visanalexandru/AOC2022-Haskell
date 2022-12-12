import Data.List -- for sort

parseInput :: [String] -> [[Int]]
parseInput [] = [[]]
parseInput ("":xs) = [] : parseInput xs
parseInput (x:xs) =  (read x:a):b
 where a:b = parseInput xs

solve1 :: [[Int]] -> Int
solve1 x = maximum (map sum x)

solve2 :: [[Int]] -> Int
solve2 x = sum $ take 3 $ reverse $ sort $ map sum x


main = do
 input <- readFile("input")

 let x =  lines input
 let input = parseInput x

 putStrLn $ show $ solve1 input
 putStrLn $ show $ solve2 input
