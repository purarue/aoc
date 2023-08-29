import           System.Environment             ( getArgs )

calculate :: Int -> Int
calculate n = -2 + quot n 3

sigmaCalculate :: Int -> Int
sigmaCalculate n = sigmaHelper n 0

sigmaHelper :: Int -> Int -> Int
sigmaHelper n acc | val <= 0  = acc
                  | otherwise = sigmaHelper val (acc + val)
  where val = calculate n

main :: IO ()
main = do
  args     <- getArgs
  contents <- readFile $ head args
  print $ sum $ map (sigmaCalculate . read) . lines $ contents
