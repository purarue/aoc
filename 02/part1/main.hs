import System.Environment (getArgs)

-- helper function so I can use words to split the string
replaceCommas :: Char -> Char
replaceCommas ',' = ' '
replaceCommas c = c

-- convert string into list of integers
decodeInput :: String -> [Int]
decodeInput inputStr = map (read::String->Int) $ words $ map replaceCommas inputStr

-- restore gravity assist program
fixInput :: [Int] -> [Int]
fixInput xs = replaceNth 2 2 $ replaceNth 12 1 xs

-- replace the nth element in the list
replaceNth :: a -> Int -> [a] -> [a]
replaceNth new index list = 
  let
    (h,_:t) = splitAt index list
  in
  h ++ new : t

-- run the calculation
calculate :: [Int] -> Int -> [Int]
calculate xs ind
  | opcode == 1 = calculate (replaceNth (xs!!ind1 + xs!!ind2) indout xs) nextind
  | opcode == 2 = calculate (replaceNth (xs!!ind1 * xs!!ind2) indout xs) nextind
  | opcode == 99 = xs
  | otherwise = error (show opcode)
  where
    opcode = xs!!ind
    ind1 = xs!!(ind + 1)
    ind2 = xs!!(ind + 2)
    indout = xs!!(ind + 3)
    nextind = ind + 4

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print $ head $ calculate (fixInput $ decodeInput contents) 0

