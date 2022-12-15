num_stacks = 9 
num_levels = 8 

-- converts a string into a line into a list of tokens (characters) 
extract :: String -> String 
extract (_:' ':_:_:xs) = ' ' : (extract xs)
extract (_:x:_:_:xs) = x : (extract xs)
extract (_:x:_:[]) = [x] 
extract [] = [] 

data Stack = Stack {uid:: Int,  content:: String} deriving Show
data Move = Move {count:: Int , from :: Int, to :: Int} deriving Show

-- converts the input into the stacks
get_stacks :: [String] -> [Stack]
get_stacks model = [Stack (x+1) (get_column x)| x<-[0.. length $ head model]]
 where get_column p = filter (/=' ') $  [x!!p | x<-model ]

parse_move :: String -> Move 
parse_move x = case w of [_,count,_,from,_,to] -> (Move (read count) (read from) (read to))
 where w = words x

-- finds the stack with the given number
find_stack :: [Stack] -> Int -> Stack
find_stack s i =  head $ filter (\x -> uid x == i) s

-- makes a move
move1 :: [Stack] -> Move -> [Stack]
move1 stacks (Move cnt from to) = new_a : new_b : other 
 where 
  a = find_stack stacks from
  b = find_stack stacks to 
  taken = reverse (take cnt $ content a)

  new_a = a{content = drop cnt $ content a}
  new_b = b{content = (taken ++ content b) }

  other = [x | x<-stacks , uid x /= from, uid x /=to]
  first = take cnt 

-- same as move1 but don't reverse the order of the boxes.
move2 :: [Stack] -> Move -> [Stack]
move2 stacks (Move cnt from to) = new_a : new_b : other 
 where 
  a = find_stack stacks from
  b = find_stack stacks to 
  taken = take cnt $ content a

  new_a = a{content = drop cnt $ content a}
  new_b = b{content = (taken ++ content b) }

  other = [x | x<-stacks , uid x /= from, uid x /=to]
  first = take cnt 


main = do 
 file <- readFile "input"
 let l = lines file
 let lines = map extract (take num_levels l)
 let stacks = get_stacks lines 
 let moves = map parse_move (drop (num_levels+2) l)
 let end_stacks1= foldl move1 stacks moves 
 let end_stacks2= foldl move2 stacks moves 
 let result1 = map (head.content) [find_stack end_stacks1 x | x<-[1..num_stacks]]
 let result2 = map (head.content) [find_stack end_stacks2 x | x<-[1..num_stacks]]
 putStrLn $ show result1 
 putStrLn $ show result2
