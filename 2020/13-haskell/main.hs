import           System.Environment             ( getArgs )

splitCommas :: String -> [String]
splitCommas buf =
  let replace ',' = ' '
      replace c   = c
  in  words $ map replace buf

-- get the index and value which corresponds to the max value in a list
maxWithIndex :: [Integer] -> (Integer, Integer)
maxWithIndex lst =
  foldr1 (\(i, x) (j, y) -> if x >= y then (i, x) else (j, y)) $ zip [0 ..] lst

part1 :: Integer -> [Integer] -> Integer -> Integer
part1 target busIds multiplier
  | waitTime >= target = (waitTime - target) * (busIds !! fromIntegral maxIndex)
  | otherwise          = part1 target busIds (multiplier + 1)
 where
  newIds               = map (* multiplier) busIds
  (maxIndex, waitTime) = maxWithIndex newIds

main :: IO ()
main = do
  -- parse
  args     <- getArgs
  contents <- readFile $ head args
  let inputLines = lines contents
  let target     = read $ head inputLines
  let busIdsIndex =
        map (\(i, c) -> (i, read c))
          $ filter (\(i, c) -> c /= "x")
          $ zip [0 ..]
          $ splitCommas
          $ last inputLines
  let busIds = map snd busIdsIndex
  putStr "Part 1: "
  print $ part1 target busIds 0

