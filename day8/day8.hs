import Data.Char 
import Data.Monoid

data Coord = Coord Int Int deriving Show


toDigit :: Char -> Int
toDigit x = ord x - ord '0'

parseLine :: String -> [Int]
parseLine x = map toDigit x

visible :: [[Int]] -> Coord -> Bool
visible grid (Coord x y) = visible up || visible down || visible left || visible right  
 where 
  width = length $ grid 
  height = length $ head grid 
  value = (grid !! y) !! x
  left = [(grid !! y) !! p | p<-[0..x-1]]
  right = [(grid !! y) !! p | p<-[x+1 .. width-1]]
  up = [(grid !! p) !! x | p<-[0 .. y-1]]
  down = [(grid !! p) !! x | p<-[y+1 .. height-1]]
  visible dir = getAll (foldMap (All.(\x-> x < value)) dir) 

score :: [[Int]] -> Coord -> Int 
score grid (Coord x y) = score up * score down * score left * score right  
 where 
  width = length $ grid 
  height = length $ head grid 
  value = (grid !! y) !! x
  left = reverse [(grid !! y) !! p | p<-[0..x-1]]
  right = [(grid !! y) !! p | p<-[x+1 .. width-1]]
  up = reverse [(grid !! p) !! x | p<-[0 .. y-1]]
  down = [(grid !! p) !! x | p<-[y+1 .. height-1]]
  score dir = let res =  (length $ takeWhile (<value) dir) in if res == length dir then res else res+1


numVisible :: [[Int]] -> Int
numVisible grid = length [coord | x<-[0..width-1] , y<-[0..height-1], let coord = (Coord x y), visible grid coord ]
 where
  width = length $ grid 
  height = length $ head grid 

maxScore :: [[Int]] -> Int
maxScore grid = maximum [score grid coord | x<-[0..width-1] , y<-[0..height-1], let coord = (Coord x y)]
 where
  width = length $ grid 
  height = length $ head grid 

main = do 
 input <- readFile "input"
 let l = map parseLine $ lines input
 putStrLn $ show $ numVisible l 
 putStrLn $ show $ maxScore l 
