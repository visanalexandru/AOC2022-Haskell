split ::  Char -> String -> [String]
split  c [] = [[]]
split  c (x:xs) = if x == c then []:next  else (x:a):b
 where next@(a:b) = split c xs 

data Interval = Interval Int Int 

split_line :: String -> [Interval]
split_line l = [Interval (read $ head first) (read $ last first), Interval (read $ head second) (read $ last second)] 
 where 
 a = split ',' l
 first = split '-' $ head a
 second = split '-' $ last a

contains  :: [Interval] -> Bool 
contains ((Interval a b):(Interval c d):[])
 | a>=c && b<=d = True
 | c>=a && d<=b = True
 | otherwise = False

overlap :: [Interval] -> Bool 
overlap ((Interval a b):(Interval c d) : []) 
 | d < a = False 
 | b < c = False 
 | otherwise = True 



main = do 
 input <- readFile "input"
 let l = map split_line $ lines input
 let ans1 = length $ filter contains l
 let ans2 = length $ filter overlap l
 
 putStrLn $ show ans1 
 putStrLn $ show ans2
