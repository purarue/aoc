import           System.Environment             ( getArgs )
import           Data.Maybe

-- helper function so I can use words to split the string
replaceCommas :: Char -> Char
replaceCommas ',' = ' '
replaceCommas c   = c

-- convert string into list of integers
decodeInput :: String -> [Int]
decodeInput inputStr =
  map (read :: String -> Int) $ words $ map replaceCommas inputStr

-- test memory with some noun/verb
testInput :: Int -> Int -> [Int] -> [Int]
testInput noun verb xs = replaceNth verb 2 $ replaceNth noun 1 xs

-- replace the nth element in the list
replaceNth :: a -> Int -> [a] -> [a]
replaceNth new index list =
  let (h, _ : t) = splitAt index list in h ++ new : t

-- run the calculation
calculate :: [Int] -> Int -> [Int]
calculate xs ind
  | opcode == 1 = calculate (replaceNth (xs !! ind1 + xs !! ind2) indout xs)
                            nextind
  | opcode == 2 = calculate (replaceNth (xs !! ind1 * xs !! ind2) indout xs)
                            nextind
  | opcode == 99 = xs
  | otherwise = error (show opcode)
 where
  opcode  = xs !! ind
  ind1    = xs !! (ind + 1)
  ind2    = xs !! (ind + 2)
  indout  = xs !! (ind + 3)
  nextind = ind + 4

-- check result
checkMemory :: Int -> Int -> [Int] -> Maybe Int
checkMemory noun verb xs | head result == 19690720 = Just (100 * noun + verb)
                         | otherwise               = Nothing
 where
  memory = testInput noun verb xs
  result = calculate memory 0

main :: IO ()
main = do
  args     <- getArgs
  contents <- readFile $ head args
  let originalContents = decodeInput contents
  let (Just result) = head $ dropWhile
        isNothing
        [ checkMemory x y originalContents | x <- [0 .. 99], y <- [0 .. 99] ]
  print result
