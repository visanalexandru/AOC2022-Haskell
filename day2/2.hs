shape_score :: String -> Int
shape_score "X" = 1
shape_score "Y" = 2 
shape_score "Z" = 3 


outcome_score :: (String,String) -> Int
outcome_score ("C","X") = 6
outcome_score ("A","Y") = 6
outcome_score ("B","Z") = 6
outcome_score ("A","X") = 3 
outcome_score ("B","Y") = 3
outcome_score ("C","Z") = 3
outcome_score (_,_) = 0 

to_play :: (String, String) -> String
to_play ("A","X") = "Z"
to_play ("A","Y") = "X"
to_play ("A","Z") = "Y"
to_play ("B","X") = "X"
to_play ("B","Y") = "Y"
to_play ("B","Z") = "Z"
to_play ("C","X") = "Y"
to_play ("C","Y") = "Z"
to_play ("C","Z") = "X"

score1 :: (String,String) -> Int
score1 c@(a,b) = outcome_score c + shape_score b

score2 :: (String,String) -> Int
score2 c@(a,b) = outcome_score (a,play) + shape_score play
 where play = to_play (a,b)

parse :: [String] -> [(String,String)]
parse [] = []
parse (x:xs) = (head w , last w) : (parse xs)
 where w = words x 

solve1 :: [(String,String)] -> Int
solve1 moves = sum $ map score1 moves

solve2 :: [(String,String)] -> Int
solve2 moves = sum $ map score2 moves


main = do 
 input <- readFile "input"
 let x = parse $ lines input
 putStrLn $ show $ solve1 x 
 putStrLn $ show $ solve2 x 
