max_size = 100000
needed = 30000000
total_disk = 70000000

data Directory = Directory Int [Directory] deriving Show

getSize :: Directory -> Int
getSize x = let (Directory a _) = x in a 

addDirectory :: Directory -> Directory -> Directory
addDirectory (Directory a b) c@(Directory p q) = Directory (a+p) (c:b)

takeLs :: [String] -> Int
takeLs [] = 0 
takeLs (('$':_):xs) = 0 
takeLs (x:xs) = (extract_size x) + (takeLs xs)

dropLs :: [String] -> [String]
dropLs [] = []
dropLs c@(('$':_):xs) = c 
dropLs (x:xs) = (dropLs xs)

extract_size :: String -> Int
extract_size x = case w of 
  ("dir":_) -> 0
  (x:_) -> read x 
 where w = words x 

getDirectories :: [String] -> [Directory] -> Directory

getDirectories [] (a : []) = a 
getDirectories [] (a: b : xs) = getDirectories [] ((addDirectory b a) :xs)

getDirectories (x:xs) (p:[]) = case words x of 
  ("$":"cd" : name:[]) -> getDirectories (dropLs $ drop 1 $ xs) ((Directory size []):p:[]) 
 where 
  size = takeLs $ drop 1 $ xs

getDirectories (x:xs) path@(p:q:ps) = case words x of 
  ("$":"cd" : "..":[]) -> (getDirectories (xs) ((addDirectory q p):ps))
  ("$":"cd" : name:[]) -> getDirectories (dropLs $ drop 1 $ xs) ((Directory size []):path) 
 where 
  size = takeLs $ drop 1 $ xs

ans1 :: Directory -> Int 
ans1 (Directory size next) = if size <= max_size then p+size else p 
 where p = sum [ans1 dir | dir <- next]

ans2 :: Directory -> Int -> Int
ans2 (Directory size next) to_delete = if size >= to_delete then (min size p) else p
 where p = foldr min (maxBound::Int) [ans2 dir to_delete| dir<-next]

main = do 
 input <- readFile "input"
 let l = lines input
 let dirs = getDirectories l [Directory 0 []] 

 let remaining = total_disk - (getSize dirs)
 let to_delete = needed - remaining 

 putStrLn $ show $ ans1 dirs
 putStrLn $ show $ ans2 dirs to_delete 

