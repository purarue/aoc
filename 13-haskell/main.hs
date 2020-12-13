import System.Environment (getArgs)

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
  | waitTime >= target = (waitTime - target) * (busIds !! (fromIntegral maxIndex))
  | otherwise = part1 target busIds (multiplier + 1)
 where
  newIds               = map (* multiplier) busIds
  (maxIndex, waitTime) = maxWithIndex newIds

-- solves by brute force
part2 :: [(Integer, Integer)] -> Integer -> Integer -> Integer
part2 busIdsIndex current skipBy
  | and existsAtOffset = current
  | otherwise          = part2 busIdsIndex (current + skipBy) skipBy
 where
    -- compute the required target values, by adding current value to the index
  targets = map ((+ current) . fst) busIdsIndex
  -- check if the target value % busId == 0,
  -- which means its divisible;
  -- it departed at that time
  existsAtOffset =
    map (\(tar, i) -> tar `rem` (snd $ busIdsIndex !! i) == 0)
      $ zip targets [0 ..]

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
  print $ part1 target busIds 0
  -- brute force solution, takes ages
  print $ part2 busIdsIndex 0 $ head busIds

